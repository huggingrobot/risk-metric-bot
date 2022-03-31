import logging
import os
import queue
import threading

import time
from datetime import datetime, timezone
from typing import Set

from dotenv import load_dotenv
from telegram import Update
from telegram.ext import CommandHandler, Updater, Dispatcher, MessageHandler, Filters, CallbackContext

from core.logging import telegram_logger as logger
from core.singleton import Singleton
from custom_types.controller_type import EMode
from settings import TELEGRAM_MODE

# from utils.event_emitter import ee, ETelegramEvent

load_dotenv()
TELEGRAM_API_KEY = os.getenv('TELEGRAM_API_KEY')
DEFAULT_TELEGRAM_NOTIFICATION_ID = os.getenv('DEFAULT_TELEGRAM_NOTIFICATION_ID')
my_queue = queue.Queue()

class TelegramBot(metaclass=Singleton):
    dispatcher: Dispatcher
    updater: Updater
    chat_list: Set = set[469591760, 1746016549]
    start_time = datetime.now(timezone.utc)
    queued_msg_started = False

    def start_bot(self):
        date = datetime.now(timezone.utc)
        logger.info(f'{date:%Y-%m-%d %H:%M:%S} > START TELEGRAM_BOT')
        self.updater = Updater(TELEGRAM_API_KEY, use_context=True)
        self.dispatcher = self.updater.dispatcher

        # Commands
        self.dispatcher.add_handler(CommandHandler('start', self.start_command))
        self.dispatcher.add_handler(CommandHandler('help', self.help_command))
        self.dispatcher.add_handler(CommandHandler('test', self.test_command))
        self.dispatcher.add_handler(CommandHandler('stats', self.stats_command))
        self.dispatcher.add_handler(CommandHandler('custom', self.custom_command))

        # Messages
        self.dispatcher.add_handler(MessageHandler(Filters.text, self.handle_message))

        # Log all errors
        self.dispatcher.add_error_handler(self.error)

    def start_command(self, update, context):
        logger.info(update.message.chat.username)
        logger.info(update.message.chat.id)
        self.chat_list.add(update.message.chat.id)
        update.message.reply_text('Hello there! I\'m a bot. What\'s up?')

    def stats_command(self, update, context):
        text = str(update.message.text).lower()
        logger.info(f'User ({update.message.chat.id}) says: {text}')
        update.message.reply_text('CHECKING STATS...')
        # ee.emit(ETelegramEvent.STATS, update.message.chat.id)

    def help_command(self, update, context):
        logger.info(update.message)
        msg = (f"/help for help message\n"
               f"/stats for bot stats\n"
               f"/test to check telegram bot")
        update.message.reply_text(msg)

    def custom_command(self, update, context):
        update.message.reply_text(
            'This is a custom command, you can add whatever text you want here.')

    def test_command(self, update, context):
        text = str(update.message.text).lower()
        logger.info(f'User ({update.message.chat.id}) says: {text}')
        update.message.reply_text('Test successful...BOT RUNNING')

    def handle_message(self, update: Update, context: CallbackContext):
        text = str(update.message.text).lower()
        logger.info(f'User ({update.message.chat.id}) says: {text}')
        update.message.reply_text(update.message.text)

    def error(self, update, context):
        # Logs errors
        logger.error(f'Update {update} caused error {context.error}')

    def send_message(self, chat_id=DEFAULT_TELEGRAM_NOTIFICATION_ID, message="blank"):
        if TELEGRAM_MODE == EMode.PRODUCTION:
            item = {"chat_id": chat_id, "message": message}
            my_queue.put(item)

        # logger.info(f'Remaining in queue: {my_queue.qsize()}')
        if not self.queued_msg_started:
            self.queued_msg_started = True
            my_thread = threading.Thread(target=self.execute_queue)
            my_thread.start()

    def execute_queue(self):
        while not my_queue.empty():
            item = my_queue.get()
            # logger.info(item)
            self.send_message_in_queue(item['chat_id'], item['message'])
            logger.info(f'Remaining in queue: {my_queue.qsize()}')
            time.sleep(2)
        self.queued_msg_started = False
        logger.info(f'End of queue: {my_queue.qsize()}')

    def send_message_in_queue(self, chat_id=DEFAULT_TELEGRAM_NOTIFICATION_ID, message="blank"):
        sleep = 1
        retry = 0
        while True and TELEGRAM_MODE == EMode.PRODUCTION:
            try:
                self.dispatcher.bot.send_message(chat_id=chat_id, text="<pre>" + message + "</pre>", parse_mode="HTML")
            except Exception as ex:
                print(f"TELEGRAM SEND FAILED: {retry}... sleeping for {sleep} second(s)\n"
                      f"{ex}")
                time.sleep(sleep)
                retry += 1
                sleep += 2
                continue
            break

    def run_bot(self):
        if TELEGRAM_MODE == EMode.PRODUCTION:
            self.updater.start_polling()


telegram_bot: TelegramBot = TelegramBot()

if __name__ == '__main__':
    logging.basicConfig(
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s', level=logging.INFO)
    logging.info("start TelegramBot.py")
    telegram_bot: TelegramBot = TelegramBot()
    telegram_bot.start_bot()
    telegram_bot.run_bot()
    msg = "🦺TEST TELEGRAM"
    telegram_bot.send_message(chat_id=469591760, message=msg)
    # telegram_bot.send_message(chat_id=-1001705426905, message=msg)
    logging.info(msg)
