"""Simple reusable UI widgets and drawing helpers."""

import pygame

from . import config


class InputBox:
    def __init__(self, rect: pygame.Rect, text: str = ""):
        self.rect = rect
        self.text = text
        self.active = False

    def handle_event(self, event: pygame.event.Event) -> None:
        if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
            self.active = self.rect.collidepoint(event.pos)

        if event.type == pygame.KEYDOWN and self.active:
            if event.key == pygame.K_BACKSPACE:
                self.text = self.text[:-1]
            elif event.key == pygame.K_RETURN:
                return
            elif len(self.text) < 24 and event.unicode.isprintable():
                self.text += event.unicode

    def draw(self, screen: pygame.Surface, font: pygame.font.Font) -> None:
        border_color = (60, 120, 220) if self.active else config.PANEL_BORDER
        pygame.draw.rect(screen, config.PANEL_COLOR, self.rect, border_radius=8)
        pygame.draw.rect(screen, border_color, self.rect, width=2, border_radius=8)

        txt_surface = font.render(self.text, True, config.TEXT_COLOR)
        screen.blit(txt_surface, (self.rect.x + 10, self.rect.y + 8))


class Button:
    def __init__(self, rect: pygame.Rect, label: str):
        self.rect = rect
        self.label = label

    def clicked(self, mouse_pos: tuple[int, int]) -> bool:
        return self.rect.collidepoint(mouse_pos)

    def draw(self, screen: pygame.Surface, font: pygame.font.Font, selected: bool = False) -> None:
        bg = (50, 118, 219) if selected else (230, 233, 238)
        fg = (255, 255, 255) if selected else config.TEXT_COLOR
        pygame.draw.rect(screen, bg, self.rect, border_radius=8)
        pygame.draw.rect(screen, config.PANEL_BORDER, self.rect, width=1, border_radius=8)

        txt = font.render(self.label, True, fg)
        txt_rect = txt.get_rect(center=self.rect.center)
        screen.blit(txt, txt_rect)


def draw_centered_lines(
    screen: pygame.Surface,
    lines: list[str],
    font: pygame.font.Font,
    center_x: int,
    start_y: int,
    line_gap: int = 10,
    color: tuple[int, int, int] = config.TEXT_COLOR,
) -> None:
    y = start_y
    for line in lines:
        txt = font.render(line, True, color)
        rect = txt.get_rect(center=(center_x, y))
        screen.blit(txt, rect)
        y += txt.get_height() + line_gap


def draw_target(screen: pygame.Surface, x: int, y: int, radius: int) -> None:
    pygame.draw.circle(screen, config.TARGET_COLOR, (x, y), radius)
