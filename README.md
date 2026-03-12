# Rapid Target Clicking Experiment

Desktop experiment prototype for a human factors study on feedback modality during rapid target clicking.

## What this app does

- Collects participant setup info (participant ID and condition: visual or auditory).
- Runs 5 practice trials and 60 experimental trials.
- Shows one circular target per trial at a random valid location.
- Ends each trial on the first click (hit or miss), logs full trial data, and advances immediately by default.
- Applies condition-specific hit feedback:
	- Visual condition: animated "HIT!!!" / "MISS..." label at the old target location.
	- Visual condition also previews the next target during feedback outside a deadzone around that label.
	- Auditory condition: distinct tones for hit and miss, with no hit/miss visual labels.
	- Auditory condition also previews the next target immediately after the click.

## Controls

- `ESC` quits the experiment at any time.
- Displays end-of-session experimental metrics and writes raw trial-level CSV data.

## Project structure

- `main.py`: program entry point.
- `experiment/config.py`: all experiment constants and editable parameters.
- `experiment/models.py`: dataclasses for participant/session and trial rows.
- `experiment/ui.py`: input box, buttons, and shared drawing helpers.
- `experiment/audio.py`: generated beep sound for auditory hit feedback.
- `experiment/experiment_app.py`: experiment flow, timing, hit detection, logging, summary, CSV export.

## Install and run

1. Create/activate a virtual environment (recommended):

```bash
python3 -m venv .venv
source .venv/bin/activate
```

2. Install dependencies:

```bash
pip install -r requirements.txt
```

3. Run the experiment:

```bash
python main.py
```

## Adjustable experiment parameters

Edit values in `experiment/config.py`:

- `PRACTICE_TRIALS`
- `EXPERIMENTAL_TRIALS`
- `TARGET_RADIUS`
- `INTER_TRIAL_INTERVAL_MS` (optional; default is 0 for no forced wait)
- `FEEDBACK_DURATION_MS`
- `FEEDBACK_LABEL_DEADZONE_RADIUS`
- `WINDOW_WIDTH` / `WINDOW_HEIGHT`

## Miss handling behavior (implemented)

The current implementation uses this consistent policy:

- Each trial ends on the first click.
- A click outside the circle is logged as a miss.
- The next trial starts immediately after feedback (unless `INTER_TRIAL_INTERVAL_MS` is set above 0).

This behavior is documented in code comments and controlled by `END_TRIAL_ON_FIRST_CLICK`.

## CSV output

CSV files are written to the `data/` folder at the end of each session.

Example CSV columns:

- `participant_id`
- `condition`
- `trial_number`
- `trial_type`
- `target_x`
- `target_y`
- `target_radius`
- `target_onset_timestamp_ms`
- `click_timestamp_ms`
- `reaction_time_ms`
- `session_elapsed_ms`
- `hit`
- `cumulative_score`

## End-of-session summary

The completion screen reports experimental trials only:

- total experimental hits
- hit rate
- mean reaction time (ms)
