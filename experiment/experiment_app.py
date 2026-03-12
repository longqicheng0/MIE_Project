from __future__ import annotations

"""Main experiment controller for rapid target clicking study."""

import csv
import math
import os
import random
import time
from datetime import datetime

import pygame

from . import config
from .audio import load_feedback_sounds
from .models import ParticipantSession, TrialResult
from .ui import Button, InputBox, draw_centered_lines, draw_target


class ExperimentApp:
    def __init__(self):
        pygame.init()
        pygame.mixer.init()

        self.screen = pygame.display.set_mode(config.WINDOW_SIZE)
        pygame.display.set_caption(config.WINDOW_TITLE)
        self.clock = pygame.time.Clock()

        self.font = pygame.font.SysFont("arial", 24)
        self.small_font = pygame.font.SysFont("arial", 20)
        self.large_font = pygame.font.SysFont("arial", 32)

        self.hit_sound, self.miss_sound = load_feedback_sounds()

        self.session: ParticipantSession | None = None
        self.results: list[TrialResult] = []
        self.score = 0
        self.session_start_timestamp_ms = 0

    def run(self) -> None:
        """Run the complete experiment lifecycle."""
        self.session = self.participant_setup_screen()

        self.set_mouse_constraint(True)
        try:
            self.show_message_screen(
                title="Welcome",
                body=[
                    "Click each red target as quickly and accurately as possible.",
                    "Each trial ends on your first click (hit or miss).",
                    "Condition feedback appears only for successful hits.",
                    "Press SPACE to begin practice.",
                ],
                continue_key=pygame.K_SPACE,
            )

            self.show_message_screen(
                title="Practice Block",
                body=[
                    f"You will complete {config.PRACTICE_TRIALS} practice trials.",
                    "Practice trials are excluded from final performance metrics.",
                    "Press SPACE to start practice.",
                ],
                continue_key=pygame.K_SPACE,
            )

            self.session_start_timestamp_ms = self.now_ms()

            self.run_trials(trial_type="practice", count=config.PRACTICE_TRIALS)

            self.show_message_screen(
                title="Experimental Block",
                body=[
                    "Practice is complete.",
                    f"Now starting {config.EXPERIMENTAL_TRIALS} real trials.",
                    "Press SPACE when ready.",
                ],
                continue_key=pygame.K_SPACE,
            )

            self.run_trials(trial_type="experimental", count=config.EXPERIMENTAL_TRIALS)

            csv_path = self.export_csv()
            summary = self.calculate_summary()
            self.show_completion_screen(summary, csv_path)
        finally:
            self.set_mouse_constraint(False)
            pygame.quit()

    @staticmethod
    def set_mouse_constraint(enabled: bool) -> None:
        """Grab/release input so cursor stays within the app while active."""
        pygame.event.set_grab(enabled)

    def participant_setup_screen(self) -> ParticipantSession:
        """Collect participant ID and condition assignment before the experiment starts."""
        input_box = InputBox(pygame.Rect(350, 250, 300, 44))
        visual_button = Button(pygame.Rect(350, 330, 140, 44), "Visual")
        auditory_button = Button(pygame.Rect(510, 330, 140, 44), "Auditory")
        start_button = Button(pygame.Rect(430, 420, 140, 48), "Start")

        selected_condition = "visual"
        error_text = ""

        running = True
        while running:
            self.clock.tick(config.FPS)

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    raise SystemExit
                if event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
                    pygame.quit()
                    raise SystemExit

                input_box.handle_event(event)

                if event.type == pygame.MOUSEBUTTONDOWN:
                    if visual_button.clicked(event.pos):
                        selected_condition = "visual"
                    elif auditory_button.clicked(event.pos):
                        selected_condition = "auditory"
                    elif start_button.clicked(event.pos):
                        participant_id = input_box.text.strip()
                        if participant_id:
                            return ParticipantSession(participant_id=participant_id, condition=selected_condition)
                        error_text = "Participant ID is required."

                if event.type == pygame.KEYDOWN and event.key == pygame.K_RETURN:
                    participant_id = input_box.text.strip()
                    if participant_id:
                        return ParticipantSession(participant_id=participant_id, condition=selected_condition)
                    error_text = "Participant ID is required."

            self.screen.fill(config.BG_COLOR)

            draw_centered_lines(
                self.screen,
                [
                    "Rapid Target Clicking Study",
                    "Enter participant information to begin",
                ],
                self.large_font,
                center_x=config.WINDOW_WIDTH // 2,
                start_y=90,
                line_gap=12,
            )

            label = self.small_font.render("Participant ID", True, config.TEXT_COLOR)
            self.screen.blit(label, (350, 220))
            input_box.draw(self.screen, self.font)

            cond_label = self.small_font.render("Condition", True, config.TEXT_COLOR)
            self.screen.blit(cond_label, (350, 300))
            visual_button.draw(self.screen, self.small_font, selected=selected_condition == "visual")
            auditory_button.draw(self.screen, self.small_font, selected=selected_condition == "auditory")
            start_button.draw(self.screen, self.small_font)

            if error_text:
                err = self.small_font.render(error_text, True, (180, 30, 30))
                self.screen.blit(err, (350, 480))

            pygame.display.flip()

        return ParticipantSession(participant_id="", condition="visual")

    def show_message_screen(self, title: str, body: list[str], continue_key: int) -> None:
        """Show instruction/interstitial screens until the user presses the requested key."""
        waiting = True
        while waiting:
            self.clock.tick(config.FPS)
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    raise SystemExit
                if event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
                    pygame.quit()
                    raise SystemExit
                if event.type == pygame.KEYDOWN and event.key == continue_key:
                    waiting = False

            self.screen.fill(config.BG_COLOR)
            title_surf = self.large_font.render(title, True, config.TEXT_COLOR)
            title_rect = title_surf.get_rect(center=(config.WINDOW_WIDTH // 2, 160))
            self.screen.blit(title_surf, title_rect)

            draw_centered_lines(
                self.screen,
                body,
                self.font,
                center_x=config.WINDOW_WIDTH // 2,
                start_y=260,
                line_gap=12,
                color=config.MUTED_TEXT_COLOR,
            )
            pygame.display.flip()

    def run_trials(self, trial_type: str, count: int) -> None:
        """Run a trial block; trials advance immediately unless ITI > 0 in config."""
        next_target_preview: tuple[int, int, int] | None = None
        for block_index in range(1, count + 1):
            global_trial_number = len(self.results) + 1
            has_next_trial = block_index < count
            next_target_preview = self.run_single_trial(
                trial_type,
                block_index,
                count,
                global_trial_number,
                forced_target=next_target_preview,
                has_next_trial=has_next_trial,
            )
            if config.INTER_TRIAL_INTERVAL_MS > 0:
                pygame.time.delay(config.INTER_TRIAL_INTERVAL_MS)

    def run_single_trial(
        self,
        trial_type: str,
        block_index: int,
        block_count: int,
        global_trial_number: int,
        forced_target: tuple[int, int, int] | None,
        has_next_trial: bool,
    ) -> tuple[int, int, int] | None:
        """Present one target, collect first click, log trial, and show feedback.

        Mouse-down events are cleared at trial start so stale clicks from the previous
        trial/feedback period do not get counted as immediate misses.
        """
        pygame.event.clear(pygame.MOUSEBUTTONDOWN)

        x, y, radius = forced_target if forced_target else self.generate_target()
        onset_timestamp_ms = self.now_ms()

        clicked = False
        while not clicked:
            self.clock.tick(config.FPS)
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    raise SystemExit
                if event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
                    pygame.quit()
                    raise SystemExit

                if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                    click_timestamp_ms = self.now_ms()
                    reaction_time_ms = click_timestamp_ms - onset_timestamp_ms
                    session_elapsed_ms = click_timestamp_ms - self.session_start_timestamp_ms
                    hit = int(self.is_hit(event.pos[0], event.pos[1], x, y, radius))

                    if hit:
                        self.score += 1

                    result = TrialResult(
                        participant_id=self.session.participant_id,
                        condition=self.session.condition,
                        trial_number=global_trial_number,
                        trial_type=trial_type,
                        target_x=x,
                        target_y=y,
                        target_radius=radius,
                        target_onset_timestamp_ms=onset_timestamp_ms,
                        click_timestamp_ms=click_timestamp_ms,
                        reaction_time_ms=reaction_time_ms,
                        session_elapsed_ms=session_elapsed_ms,
                        hit=hit,
                        cumulative_score=self.score,
                    )
                    self.results.append(result)

                    next_target_preview = self.present_feedback(
                        hit=hit,
                        previous_target=(x, y, radius),
                        has_next_trial=has_next_trial,
                    )
                    clicked = True
                    return next_target_preview

            self.draw_trial_screen(
                trial_type=trial_type,
                block_index=block_index,
                block_count=block_count,
                x=x,
                y=y,
                radius=radius,
            )
            pygame.display.flip()

        return None

    def draw_trial_screen(
        self, trial_type: str, block_index: int, block_count: int, x: int, y: int, radius: int
    ) -> None:
        """Render target, score, and trial progress."""
        self.screen.fill(config.BG_COLOR)
        draw_target(self.screen, x, y, radius)

        progress_prefix = "Practice" if trial_type == "practice" else "Trial"
        progress_text = f"{progress_prefix} {block_index}/{block_count}"
        score_text = f"Score: {self.score}"
        elapsed_ms = self.now_ms() - self.session_start_timestamp_ms if self.session_start_timestamp_ms else 0
        elapsed_text = f"Time: {self.format_time_ms(elapsed_ms)}"

        progress_surf = self.small_font.render(progress_text, True, config.TEXT_COLOR)
        score_surf = self.small_font.render(score_text, True, config.TEXT_COLOR)
        elapsed_surf = self.small_font.render(elapsed_text, True, config.TEXT_COLOR)

        self.screen.blit(progress_surf, (20, 16))
        self.screen.blit(elapsed_surf, ((config.WINDOW_WIDTH - elapsed_surf.get_width()) // 2, 16))
        self.screen.blit(score_surf, (config.WINDOW_WIDTH - score_surf.get_width() - 20, 16))

    @staticmethod
    def is_hit(click_x: int, click_y: int, target_x: int, target_y: int, radius: int) -> bool:
        """Return True if click falls inside the target circle."""
        dist = math.sqrt((click_x - target_x) ** 2 + (click_y - target_y) ** 2)
        return dist <= radius

    def present_feedback(
        self,
        hit: int,
        previous_target: tuple[int, int, int],
        has_next_trial: bool,
    ) -> tuple[int, int, int] | None:
        """Present post-click feedback and optionally preview next target in visual condition."""
        previous_x, previous_y, _ = previous_target

        next_target_preview = None
        if has_next_trial and self.session.condition in {"visual", "auditory"}:
            next_target_preview = self.generate_target(
                excluded_zone=(
                    previous_x,
                    previous_y,
                    config.FEEDBACK_LABEL_DEADZONE_RADIUS,
                )
            )

        if self.session.condition == "auditory":
            if hit:
                self.hit_sound.play()
            else:
                self.miss_sound.play()

        start_ms = self.now_ms()
        while self.now_ms() - start_ms < config.FEEDBACK_DURATION_MS:
            self.clock.tick(config.FPS)
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    raise SystemExit
                if event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
                    pygame.quit()
                    raise SystemExit

            self.screen.fill(config.BG_COLOR)

            if next_target_preview:
                nx, ny, nr = next_target_preview
                draw_target(self.screen, nx, ny, nr)

            if self.session.condition == "visual":
                # Animate the feedback label at the old target location.
                text = "HIT!!!" if hit else "MISS..."
                color = (30, 140, 40) if hit else (180, 40, 40)

                elapsed = self.now_ms() - start_ms
                progress = max(0.0, min(1.0, elapsed / config.FEEDBACK_DURATION_MS))
                pulse = 1.0 + 0.20 * math.sin(progress * math.pi * 3.0)
                label_font = pygame.font.SysFont("arial", max(24, int(42 * pulse)))
                label = label_font.render(text, True, color)
                label_rect = label.get_rect(center=(previous_x, previous_y))
                self.screen.blit(label, label_rect)
            # Auditory condition intentionally shows no hit/miss label.

            score_text = self.small_font.render(f"Score: {self.score}", True, config.TEXT_COLOR)
            elapsed_ms = self.now_ms() - self.session_start_timestamp_ms if self.session_start_timestamp_ms else 0
            elapsed_text = self.small_font.render(
                f"Time: {self.format_time_ms(elapsed_ms)}",
                True,
                config.TEXT_COLOR,
            )
            self.screen.blit(
                elapsed_text,
                ((config.WINDOW_WIDTH - elapsed_text.get_width()) // 2, 16),
            )
            self.screen.blit(score_text, (config.WINDOW_WIDTH - score_text.get_width() - 20, 16))

            pygame.display.flip()

        pygame.event.clear(pygame.MOUSEBUTTONDOWN)
        return next_target_preview

    def generate_target(
        self,
        excluded_zone: tuple[int, int, int] | None = None,
    ) -> tuple[int, int, int]:
        """Generate a random target location fully inside bounds and outside optional deadzone."""
        radius = config.TARGET_RADIUS
        min_x = radius
        max_x = config.WINDOW_WIDTH - radius
        min_y = radius + 50  # keep clear space for status text
        max_y = config.WINDOW_HEIGHT - radius

        for _ in range(300):
            x = random.randint(min_x, max_x)
            y = random.randint(min_y, max_y)

            if excluded_zone is None:
                return x, y, radius

            ex, ey, deadzone_radius = excluded_zone
            distance = math.sqrt((x - ex) ** 2 + (y - ey) ** 2)
            if distance > (deadzone_radius + radius):
                return x, y, radius

        # Fallback: if random placement repeatedly fails, accept first valid bounds point.
        return random.randint(min_x, max_x), random.randint(min_y, max_y), radius

    def calculate_summary(self) -> dict[str, float]:
        """Compute end-of-session metrics for experimental trials only."""
        exp_trials = [r for r in self.results if r.trial_type == "experimental"]

        if not exp_trials:
            return {
                "total_hits": 0,
                "hit_rate": 0.0,
                "mean_rt_ms": 0.0,
            }

        total_hits = sum(r.hit for r in exp_trials)
        hit_rate = (total_hits / len(exp_trials)) * 100.0
        mean_rt_ms = sum(r.reaction_time_ms for r in exp_trials) / len(exp_trials)

        return {
            "total_hits": total_hits,
            "hit_rate": hit_rate,
            "mean_rt_ms": mean_rt_ms,
        }

    def export_csv(self) -> str:
        """Write trial-level data to CSV and return output file path."""
        os.makedirs(config.OUTPUT_DIR, exist_ok=True)
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"participant_{self.session.participant_id}_{timestamp}.csv"
        output_path = os.path.join(config.OUTPUT_DIR, filename)

        fieldnames = [
            "participant_id",
            "condition",
            "trial_number",
            "trial_type",
            "target_x",
            "target_y",
            "target_radius",
            "target_onset_timestamp_ms",
            "click_timestamp_ms",
            "reaction_time_ms",
            "session_elapsed_ms",
            "hit",
            "cumulative_score",
        ]

        with open(output_path, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            for row in self.results:
                writer.writerow(row.__dict__)

        return output_path

    def show_completion_screen(self, summary: dict[str, float], csv_path: str) -> None:
        """Display final metrics and where the CSV was saved."""
        waiting = True
        total_session_ms = self.results[-1].session_elapsed_ms if self.results else 0
        lines = [
            f"Total experimental hits: {int(summary['total_hits'])}/{config.EXPERIMENTAL_TRIALS}",
            f"Experimental hit rate: {summary['hit_rate']:.1f}%",
            f"Mean RT (experimental): {summary['mean_rt_ms']:.1f} ms",
            f"Total task time: {self.format_time_ms(total_session_ms)}",
            f"CSV saved to: {csv_path}",
            "Press ESC to close.",
        ]

        while waiting:
            self.clock.tick(config.FPS)
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    waiting = False
                elif event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
                    waiting = False

            self.screen.fill(config.BG_COLOR)
            title = self.large_font.render("Session Complete", True, config.TEXT_COLOR)
            title_rect = title.get_rect(center=(config.WINDOW_WIDTH // 2, 170))
            self.screen.blit(title, title_rect)

            draw_centered_lines(
                self.screen,
                lines,
                self.font,
                center_x=config.WINDOW_WIDTH // 2,
                start_y=260,
                line_gap=14,
            )
            pygame.display.flip()

    @staticmethod
    def now_ms() -> int:
        return int(time.time() * 1000)

    @staticmethod
    def format_time_ms(total_ms: int) -> str:
        """Format milliseconds as mm'ss'mmm (example: 01'04'145)."""
        total_ms = max(0, int(total_ms))
        minutes = total_ms // 60000
        seconds = (total_ms % 60000) // 1000
        milliseconds = total_ms % 1000
        return f"{minutes:02d}'{seconds:02d}'{milliseconds:03d}"
