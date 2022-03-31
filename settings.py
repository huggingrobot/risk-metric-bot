import os
from os.path import dirname, abspath

from dotenv import load_dotenv

from custom_types.controller_type import EMode

load_dotenv('.env')

BASE_DIR = dirname(dirname(abspath(__file__)))

API_KEY = os.getenv('API_KEY')
SECRET_KEY = os.getenv('SECRET_KEY')

SYMBOLS = ['btcusdt', 'ethusdt', 'rsrusdt', 'bakeusdt', 'ksmusdt']

SUPPORTED_CURRENCIES = ['BTC', 'ETH', 'ADA']
RECORD_SECONDS_TO_KEEP = 3

DECIMALS = 1
TELEGRAM_MODE = EMode.PRODUCTION
