# StatusBot

Installing via stack:\
git clone https://github.com/omegaGreeNya/StatusBot.git \
cd StatusBot\
stack install


Usage:\
First, you will need to specify bot settings (currently for telegram). Open config.json and edit token field with your bot token value. Also, make sure that telegramUsage = true, otherwise telegram-side would not be enabled.


After that you can enable bot with:\
  StatusBot-exe\
Enter help, to get the help message.


Config fields:\
consoleLogging - for debug purposes only, with this option enabled app would log into console.\
fileLogging - then true, logs into log file.\
logFile - path to log file (file may not exist, it would be created).\
logLevel - Minimal logging level to log, messages with level less that specified would be supressed.\
  Supported log levels:  DEBUG/INFO/WARN/ERROR\
formatter - log massage formating mode\
  Supported logging formats:\
    DATE - message time and level;\
    SIMPLE - only message level;\
    NO_FORMATTING - raw log messages.\
telegramUsage - then true, app would be enabled for telegram.\
  token - telegram bot token.\
  timeout - Time interval in seconds for long polling (recomended 10-30).\
feedback - address to send user feedback to, adderess may be replied to user in case of unexpected error (Unxpected errors would be logged as ERROR though).
