import math
import re

import pandas as pd
from datetime import datetime
from time import sleep, time
from core.logging import logger
from binance.client import Client

from services.telegram_service import telegram_bot
from settings import API_KEY, SECRET_KEY

sheet_id = "1cWOz33mP-sC8gFU0J5ONSDgcAqmLXFXTUfi7LLznC7E"
sheet_name = "Dashboard"
url = f"https://docs.google.com/spreadsheets/d/{sheet_id}/gviz/tq?tqx=out:csv&sheet={sheet_name}"

RISK_METRICS = {}
HIT_TARGET = {}
SELL_TARGET = {}
MULTIPLIER_LEVEL = {"0.1": 5, "0.2": 4, "0.3": 3, "0.4": 2, "0.5": 1, "0.6": 1, "0.7": 2, "0.8": 3, "0.9": 4, "1.0": 5}

lc = re.compile('[a-z]+')

sample_balance = 100


def check_hit_target(coin_name, coin_risk):
    if round(RISK_METRICS[coin_name], 1) != round(coin_risk, 1):
        level = round(coin_risk, 1)
        msg = f"{coin_name} NEW LEVEL HIT {level}\n"


        # if HIT_TARGET[coin_name][str(level)]:
        if level >= 0.5:
            record_coin_bal(coin_name, level)
            HIT_TARGET[coin_name][str(level)] = True
            msg = f"{msg}EXECUTE SELL ORDER on {coin_name} with total amount {MULTIPLIER_LEVEL[str(level)]}x"
        else:
            HIT_TARGET[coin_name][str(level)] = True
            msg = f"{msg}EXECUTE BUY ORDER on {coin_name} with total amount {MULTIPLIER_LEVEL[str(level)]}x"

        logger.info(msg)
        telegram_bot.send_message(message=msg)


def record_coin_bal(coin_name, level):
    if level == 0.5:
        logger.info(coin_name)
        multiplier = 1
        single_part = sample_balance / 15
        SELL_TARGET[coin_name] = {}
        for i in range(5, 10, 1):
            SELL_TARGET[coin_name][str(i / 10)] = round(multiplier * single_part, 5)
            multiplier += 1
        logger.info(SELL_TARGET)

        # total = 0
        # for key, value in SELL_TARGET[coin_name].items():
        #     total += value
        # logger.info(f"{total=}")


def main():
    while True:
        df = pd.read_csv(url)
        msg = ""
        for i in range(4, 28):
            coin_name = df.iloc[i, 0]
            coin_risk = df.iloc[i, 3]
            if not lc.findall(coin_name) and not math.isnan(coin_risk):
                if coin_name not in RISK_METRICS:
                    RISK_METRICS[coin_name] = coin_risk
                    HIT_TARGET[coin_name] = {"0.1": False, "0.2": False, "0.3": False, "0.4": False, "0.5": False,
                                             "0.6": False, "0.7": False, "0.8": False, "0.9": False, "1.0": False}
                    HIT_TARGET[coin_name][str(round(coin_risk, 1))] = True
                    continue
                if RISK_METRICS[coin_name] != coin_risk:
                    check_hit_target(coin_name, coin_risk)
                    changes = "increase" if RISK_METRICS[coin_name] < coin_risk else "decrease"
                    RISK_METRICS[coin_name] = coin_risk
                    msg = f"{msg}\n{coin_name} ({coin_risk}) [{changes}]"

        if msg:
            logger.info(msg)
            # telegram_bot.send_message(message=msg)
        # logger.info(f"{RISK_METRICS}")
        # logger.info(f"{HIT_TARGET}")
        sleep_sec = 60 - time() % 60
        logger.info(f"{datetime.now()} {sleep_sec=}")
        sleep(sleep_sec)


if __name__ == '__main__':
    telegram_bot.start_bot()
    telegram_bot.send_message(message="Starting bot....")

    client = Client(API_KEY, SECRET_KEY, testnet=True)
    # info = client.get_all_tickers()

    # order = client.order_market_buy(
    #     symbol='XRPBUSD',
    #     quantity=100)
    # logger.info(order)

    # info = client.get_all_tickers()
    info = client.get_account()
    logger.info(info)

    main()
