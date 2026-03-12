"""Configuration constants for the target-clicking experiment."""

# Window
WINDOW_WIDTH = 1000
WINDOW_HEIGHT = 700
WINDOW_SIZE = (WINDOW_WIDTH, WINDOW_HEIGHT)
WINDOW_TITLE = "Rapid Target Clicking Experiment"
FPS = 60

# Experiment design
PRACTICE_TRIALS = 5
EXPERIMENTAL_TRIALS = 20

# Target
TARGET_RADIUS = 28
TARGET_COLOR = (220, 40, 40)

# Timing (milliseconds)
# By default there is no enforced fixed pause between trials.
INTER_TRIAL_INTERVAL_MS = 0
FEEDBACK_DURATION_MS = 450
FEEDBACK_LABEL_DEADZONE_RADIUS = 120

# UI colors
BG_COLOR = (240, 242, 245)
TEXT_COLOR = (20, 20, 20)
MUTED_TEXT_COLOR = (90, 90, 90)
PANEL_COLOR = (255, 255, 255)
PANEL_BORDER = (205, 210, 218)

# CSV output
OUTPUT_DIR = "data"

# Miss handling policy:
# This experiment ends each trial on the first click (hit or miss).
# A miss is recorded immediately and the trial advances after feedback + ITI.
END_TRIAL_ON_FIRST_CLICK = True
