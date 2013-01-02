module Main where

import Data.Bits
import Data.Maybe
import Data.List
import Network.Socket 
import Network.IRC.Base 
import Network.IRC.Commands
import Network.IRC.Parser
import System.IO
import System.Exit

data Bot = { socket :: Handle }
type IrcBot = ReaderT Bot IO

data BotHandle =
	BotHandle { bHandle  :: Handle,
							bNick 	 :: String,
							bChannel :: String }

connectBot :: HostName 		-- Remote host name
							-> String 	-- Port number / name
							-> String   -- Nickname
							-- -> [String] -- WIP List of channels to connect to
							-> String   -- Channel to connect to
							-> IO BotHandle
connectBot hostname port nickname channel =
		do 
				addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
				let serveraddr = head addrinfos

				sock <- socket (addrFamily serveraddr) Stream defaultProtocol
				setSocketOption sock KeepAlive 1

				connect sock (addrAddress serveraddr)

				h <- socketToHandle sock ReadWriteMode

				hSetBuffering h (BlockBuffering Nothing)

				let handle 	= BotHandle h nickname channel

				commandBot handle (nick (bNick handle))
				commandBot handle (user (bNick handle) (bNick handle) (bNick handle) (bNick handle))
				commandBot handle (joinChan (bChannel handle))

				return $ handle

closeBot :: BotHandle -> IO ()
closeBot bot = hClose (bHandle bot)

commandBot :: BotHandle -> Message -> IO ()
commandBot bot command = 
	do
		hPutStrLn (bHandle bot) (encode command)
		hFlush (bHandle bot)

listenBot :: BotHandle -> IO ()
listenBot bot = forever $ do
				s <- hGetLine (bHandle bot)
				let msg = decode s
				case msg of
					Just message -> do
						case (msg_command message) of
							"JOIN" -> do
									putStrLn line
								where
									channels = concat (msg_params message)
									line = "Joined " ++ channels
							"PRIVMSG" -> do
								handlePrivMsg bot (msg_params message)
							"PING" -> do
								tell bot "PONG"
							otherwise -> do
								return ()
								--putStrLn (showMessage message)
								--putStrLn (msg_command message)
								--putStrLn (showParameters (msg_params message))
					Nothing -> do
						putStrLn s
		where
			forever a = a >> forever a

handlePrivMsg :: BotHandle -> [Parameter] -> IO ()
handlePrivMsg bot params = do
			if user == (bChannel bot) then do
					eval bot message
			else do
					putStrLn user
					putStrLn message
		where
			(user:message:xs) = params

eval :: BotHandle -> String -> IO ()
eval bot "!quit" = commandBot bot (quit Nothing) >> exitWith ExitSuccess
eval bot s = do
		case command of
			"!echo" -> do
				commandBot bot (privmsg (bChannel bot) payload)
			otherwise -> do
				return ()
	where
		command = head (words s)
		payload = unwords (tail (words s))

tell :: BotHandle -> String -> IO ()
tell bot msg =
		do 
				hPutStrLn (bHandle bot) sendmsg
			 	hFlush (bHandle bot)
		where sendmsg = (bNick bot) ++ msg
