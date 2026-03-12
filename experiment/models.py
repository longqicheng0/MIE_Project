"""Data models used across the experiment."""

from dataclasses import dataclass


@dataclass
class ParticipantSession:
    participant_id: str
    condition: str  # "visual" or "auditory"


@dataclass
class TrialResult:
    participant_id: str
    condition: str
    trial_number: int
    trial_type: str
    target_x: int
    target_y: int
    target_radius: int
    target_onset_timestamp_ms: int
    click_timestamp_ms: int
    reaction_time_ms: int
    session_elapsed_ms: int
    hit: int
    cumulative_score: int
