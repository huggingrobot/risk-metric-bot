from datetime import datetime, timezone


def time_now_in_ms() -> int:
    """Returned current timestamp is offset-aware"""
    return date_to_timestamp(datetime.now(timezone.utc))


def date_to_timestamp(date: datetime) -> int:
    """Returned timestamp is offset-aware"""
    return int((date - datetime(1970, 1, 1, tzinfo=timezone.utc)).total_seconds() * (10 ** 3))
