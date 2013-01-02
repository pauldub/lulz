module Main where

import Data.Either
import Data.Maybe
import Data.List
import Data.Bson
import Network
import Network.IRC.Base 
import Network.IRC.Commands
import Network.IRC.Parser
import qualified Database.MongoDB as M
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Control.Exception
import Text.Printf
import qualified Data.Text as T
import Prelude hiding (catch)

server 		 = "irc.freenode.org"
port 			 = 6667
chan 			 = "#rbx"
nickname 	 = "denain2"
database 	 = "crawler_test"

data Bot = Bot { socket :: Handle,
								 pipe 	:: M.Pipe }
type IrcBot = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
	where
		disconnect = hClose . socket
		loop st 	 = catch (runReaderT run st) (\(SomeException _) -> return ())

connect :: IO Bot
connect = notify $ do
		h <- connectTo server (PortNumber (fromIntegral port))
		p <- M.runIOE $ M.connect $ M.host "127.0.0.1"
		--c <- M.access p M.master (T.pack "crawler_test" :: M.Database) M.allCollections
		--case c of
			--Left err -> do
				--putStrLn (show err)
			--Right collections -> do
				--putStrLn (T.unpack (T.unwords collections))
		hSetBuffering h NoBuffering
		return (Bot h p)
	where
		notify a = bracket_
			(printf "Connection to %s ..." server >> hFlush stdout)
			(putStrLn "Done")
			a

run :: IrcBot ()
run = do
	write (nick nickname)
	write (user nickname nickname nickname nickname)
	write (joinChan chan)
	asks socket >>= listen

listen :: Handle -> IrcBot ()
listen h = forever $ do
		s <- io (hGetLine h)
		let message = (decode s)
		if (isJust message) then 
			let (Just m) = message in
				handleMessage m
		else
			return () --
			--io (putStrLn (translateReply s))
	where
		forever a = a >> forever a

handleMessage :: Message -> IrcBot ()
handleMessage m = do
		h <- asks socket
		case command of
			"PING" -> do
				pong m
			"PRIVMSG" -> do
				eval m
			otherwise -> do
				return ()
	where
		command = (translateReply (msg_command m))

eval :: Message -> IrcBot ()
eval m = do
	h <- asks socket
	let prefix = (msg_prefix m)
	case prefix of
		Just prefix -> do
				case command of
					"!quit" -> do
						write (quit Nothing) >> io (exitWith ExitSuccess)
					"!echo" -> do respond channel n payload
					"!locations" -> do
							if (length (words payload)) > 0 then
								do
									case subcommand of
										"latest" -> do
											p <- asks pipe
											let run act = M.access p M.master (T.pack database :: M.Database) act
											--M.find $ do M.rest =<< (M.select [] "crawler_models_locations")

											c <- run $ M.find $ M.select [] (T.pack "crawler_models_locations") 
											case c of
												Left err -> do
													io (putStrLn (show err))
												Right cursor -> do
														docs <- run $ M.rest $ cursor
														case docs of
															Left err -> do
																io (putStrLn (show err))
															Right docs -> do
																	respond channel n (show idsStr)
																where 
																	ids = mapM (\doc -> do
																						val <- M.look (T.pack "_id") doc
																						typed val
																					) docs
																	idsStr = mapM (\id ->
																						"id: " ++ id ++ "\r\n"
																					) ids
											respond channel n "That's all!"
											--case c of
												--Left err -> do
												--Right collections -> do
													--putStrLn (T.unpack (T.unwords collections))
							else
								return ()
						where
							subcommand = head (words payload)
							query 	   = unwords (tail (words payload))
					otherwise -> do
						io (putStrLn (prettyMessage n command))
			where
				(NickName n _ _) = prefix
				params 					 = (msg_params m)
				channel 				 = head params
				message 				 = unwords (tail params)
				command 				 = head (words message)
				payload 				 = unwords (tail (words message))
		Nothing ->
			return ()

prettyLocation :: Document -> String
prettyLocation = undefined
--runP :: M.Action 
--runP act = do
	--p <- asks pipe
	--M.access p M.master database act


prettyMessage :: String -> String -> String
prettyMessage prefix m = "<-- " ++ prefix ++ ": " ++ m

pong :: Message -> IrcBot ()
pong m = do
	h <- asks socket
	io $ (hPutStrLn h ("PING:" ++ (head (msg_params m))))

respond :: String -> String -> String -> IrcBot ()
respond c n s = do
	if c == nickname then
		write (privmsg n s)
	else
		write (privmsg c s)

write :: Message -> IrcBot ()
write m = do
	h <- asks socket
	io $ hPutStrLn h (encode m)
	io (putStrLn ("--> " ++ (showMessage m)))

io :: IO a -> IrcBot a
io = liftIO

--runM act = do
	--p <- asks pipe
	--M.access p M.master (T.pack database :: M.Database) act
	--M.find $ do M.rest =<< (M.select [] "crawler_models_locations")
--runM :: M.Action -> IO a 
runM :: (IrcBot (ReaderT Bot IO  )) => M.Action m a -> IrcBot (Either M.Failure a)
runM a = do
	p <- asks pipe
	M.access p M.master (T.pack database :: M.Database) a


--getLocations :: M.Cursor -> IrcBot (Maybe [Document])
--getLocations cur = do
	--c <- io $ runM $ M.find $ M.select [] (T.pack "crawler_models_locations")
	--case c of
		--Left err -> do
			--return (Nothing)
		--Right cursor -> do
			--docs <- runM $ M.rest $ cursor
			--case docs of
				--Left err -> do
					--return (Nothing)
				--Right docs -> do
					--return (Just docs)
