"""Audio helpers for feedback tones."""

import math
import os
import struct
import tempfile
import wave

import pygame


def create_beep_wav_file(
    frequency_hz: int = 800,
    duration_ms: int = 120,
    volume: float = 0.35,
    sample_rate: int = 44100,
) -> str:
    """Create a temporary mono WAV file and return its path."""
    n_samples = int(sample_rate * (duration_ms / 1000.0))
    amplitude = int(32767 * max(0.0, min(volume, 1.0)))

    fd, path = tempfile.mkstemp(prefix="hf_beep_", suffix=".wav")
    os.close(fd)

    with wave.open(path, "w") as wav_file:
        wav_file.setnchannels(1)
        wav_file.setsampwidth(2)
        wav_file.setframerate(sample_rate)

        for i in range(n_samples):
            t = i / sample_rate
            sample = int(amplitude * math.sin(2 * math.pi * frequency_hz * t))
            wav_file.writeframesraw(struct.pack("<h", sample))

    return path


def load_feedback_sounds() -> tuple[pygame.mixer.Sound, pygame.mixer.Sound]:
    """Load distinct hit/miss sounds for auditory feedback.

    Returns:
        (hit_sound, miss_sound)
    """
    hit_path = create_beep_wav_file(frequency_hz=980, duration_ms=110, volume=0.35)
    miss_path = create_beep_wav_file(frequency_hz=360, duration_ms=150, volume=0.35)
    return pygame.mixer.Sound(hit_path), pygame.mixer.Sound(miss_path)
