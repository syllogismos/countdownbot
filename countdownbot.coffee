Twit     = require 'twit'
Shell    = require 'shelljs'
minimist = require 'minimist'


argv = minimist process.argv.slice(2), default:
	verbose: false
	config: './config.json'

getConfig = (path) ->
	if path[0] != '/' and path[0..1] != './'
		path = './' + path
	require(path)

tweet = (T, status) ->
	console.log status
	unless argv.noop
		T.post 'statuses/update', status: status, (err) ->
			console.log err if err

respondToTweet = (T, status, tweet) ->
	console.log "responding to the tweet " + tweet.text
	console.log "response is " + status
	if status != "-1"
		T.post 'statuses/update', {status: status, in_reply_to_status_id:tweet.id_str}, (err) -> 
			console.log "tweet posted"


processTweet = (tweet) ->
	# given a tweet content along with the nick mention 
	# return the solution to the numbers game.
	# like removing everything except for the list of numbers
	# ideally tweet = @nick 876 2 5 8 25 7 50
	if (tweet.text.indexOf "numbersgame") == -1
		return "-1" 
	else
		numbers = extract tweet.text
		cmd = "numbersgame " + numbers
		response = "@" + tweet.user.screen_name + " " + Shell.exec(cmd).output # we are using Shell.exec synchronously for now.
		console.log "processTweet response is " + response
		return response

extract = (string) -> # http://stackoverflow.com/questions/25107905 for future
	numbers = string.match(/\b\d+\b/gi)
	return numbers.join(" ")

main = ->
	config = getConfig argv.config
	T = new Twit config.account
	me = config.nick
	stream = T.stream 'user', {track: me}, (err) ->
		console.log err if err
	stream.on 'tweet', (tweet) ->
		console.log "stream tweet event"
		console.log tweet
		response = processTweet tweet # response must start with the twitter handle of the author of the tweet we are responding to
		respondToTweet T, response, tweet
	stream.on 'warning', (err) ->
		console.log "stream warning event"
	stream.on 'connect', (err) ->
		console.log "stream connect event"
	stream.on 'reconnect', (err) ->
		console.log "stream reconnect event"
	stream.on 'connected', (err) ->
		console.log "stream connected event"
	stream.on 'error', (err) ->
		console.log "stream error event"


if require.main == module
	main()

exports.main = main
exports.extract = extract
exports.processTweet = processTweet
exports.respondToTweet = respondToTweet
exports.tweet = tweet
