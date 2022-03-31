import logging

from utils.path_utils import build_path


def setup_logging(output_path: list[str] = None):
    # Remove all handlers associated with the root logger object.
    for handler in logging.root.handlers[:]:
        logging.root.removeHandler(handler)

    handlers: list[logging.Handler] = []
    formatter = logging.Formatter('%(asctime)s [%(name)-12.12s] %(levelname)s : %(message)s')
    stream_handler = logging.StreamHandler()
    stream_handler.setFormatter(formatter)
    handlers.append(stream_handler)

    path = ['logs', 'pnd-spot-bot.log'] if output_path is None else output_path

    file_handler = logging.FileHandler(build_path(path), encoding='utf-8')
    file_handler.setLevel(logging.DEBUG)
    file_handler.setFormatter(formatter)
    handlers.append(file_handler)
    logging.basicConfig(level=logging.INFO, handlers=handlers)


def get_logger():
    return logging.getLogger('pnd-spot-bot')


def get_telegram_logger():
    _logger = logging.getLogger('telegram-bot')
    _logger.setLevel(logging.INFO)
    return _logger


setup_logging()

logger = get_logger()
telegram_logger = get_telegram_logger()
