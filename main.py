import asyncio
from concurrent.futures.thread import ThreadPoolExecutor

import math
import re

import pandas as pd
from datetime import datetime, timedelta
from time import sleep, time
from core.logging import logger
from binance.client import Client

from services.telegram_service import telegram_bot
from settings import API_KEY, SECRET_KEY, DEFAULT_TELEGRAM_NOTIFICATION_ID
from utils.event_utils import ee, TelegramEventType
from utils.math_utils import round_decimals_down

startTime = time()
sheet_id = "1cWOz33mP-sC8gFU0J5ONSDgcAqmLXFXTUfi7LLznC7E"
sheet_name = "Dashboard"
url = f"https://docs.google.com/spreadsheets/d/{sheet_id}/gviz/tq?tqx=out:csv&sheet={sheet_name}"

RISK_METRICS = {}
CURRENT_RISK = {}
LAST_HIT_RISK = {}
SELL_TARGET = {}
MULTIPLIER_LEVEL = {"0.1": 5, "0.2": 4, "0.3": 3, "0.4": 2, "0.5": 1, "0.6": 1, "0.7": 2, "0.8": 3, "0.9": 4, "1.0": 5}

lc = re.compile('[a-z]+')

sample_balance = 100


def get_uptime():
    """
    Returns the number of seconds since the program started.
    """
    # do return startTime if you just want the process start time
    return time() - startTime


def check_hit_target(coin_name, coin_risk):
    old_level = round_decimals_down(RISK_METRICS[coin_name], 1)
    new_level = round_decimals_down(coin_risk, 1)

    if old_level != new_level:
        msg = f"{coin_name} NEW LEVEL HIT {new_level}\n"
        CURRENT_RISK[coin_name] = new_level

        if str(new_level) != LAST_HIT_RISK[coin_name]:
            LAST_HIT_RISK[coin_name] = old_level

            if new_level >= 0.5:
                record_coin_bal(coin_name, new_level)
                msg = f"{msg}EXECUTE SELL ORDER on {coin_name} with total amount {MULTIPLIER_LEVEL[str(new_level)]}/15"
            else:
                msg = f"{msg}EXECUTE BUY ORDER on {coin_name} with total amount {MULTIPLIER_LEVEL[str(new_level)]}/15"

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


def stats_requested(chat_id=DEFAULT_TELEGRAM_NOTIFICATION_ID):
    td = timedelta(seconds=round(get_uptime()))
    timeup = f"{td.days}days, {(td.seconds // 3600) % 24}hrs, {(td.seconds // 60) % 60}mins, {td.seconds % 60}secs"

    msg = f"RISK METRIC\n" \
          f"================\n"
    for key, value in RISK_METRICS.items():
        msg = f"{msg}{key:<5} {value:.03f} {'SELL' if value > 0.5 else 'BUY'}\n"
    msg = f"{msg} {timeup}\n"
    logger.info(msg)
    telegram_bot.send_message(chat_id=chat_id, message=msg)


async def run_checker():
    last_checked_day = 0
    ee.on(TelegramEventType.STATS, stats_requested)
    while True:
        try:
            df = pd.read_csv(url)
            msg = ""
            for i in range(4, 28):
                coin_name = df.iloc[i, 0]
                coin_risk = df.iloc[i, 3]
                if not lc.findall(coin_name) and not math.isnan(coin_risk):
                    if coin_name not in RISK_METRICS:
                        RISK_METRICS[coin_name] = coin_risk
                        LAST_HIT_RISK[coin_name] = str(round_decimals_down(coin_risk, 1))
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
            if last_checked_day != int(datetime.now().strftime('%d')):
                logger.info(f"{last_checked_day} != {int(datetime.now().strftime('%d'))}")
                stats_requested()
            last_checked_day = int(datetime.now().strftime('%d'))

        except Exception as ex:
            msg = f"Exception just occured:\n{ex}"
            logger.info(msg)
            telegram_bot.send_message(message=msg)
        finally:
            sleep_sec = 300 - time() % 300
            logger.info(f"{datetime.now()} {sleep_sec=}")
            sleep(sleep_sec)


async def main():
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

    with ThreadPoolExecutor(max_workers=1) as executor:
        event_loop = asyncio.get_event_loop()
        await asyncio.gather(run_checker(),
                             event_loop.run_in_executor(executor, telegram_bot.run_bot))


if __name__ == '__main__':
    loop = asyncio.get_event_loop()
    try:
        asyncio.ensure_future(main())
        loop.run_forever()
    finally:
        loop.run_until_complete(loop.shutdown_asyncgens())
        loop.close()
