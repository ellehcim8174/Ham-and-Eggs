import tweepy
 
consumer_key = 'YeCwcDwyw8n74maWZlfp3PRgO'
consumer_secret = 'qoBTI00yygNoiFrBNuJxaHiNGAfKxRpUALRRMIXr0E9VjbSWti'
access_token = '4887048193-opP0qUxX0L6IEA5ze037b3CbszA8SIhi4Aw6pXb'
access_token_secret = 'sOS253OFyVm5CFrMJuh5Ia1HpFFP4IyGUvsHEdvfhfM0J'
 
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
 
api = tweepy.API(auth)
 
# api.update_status('Reflow!')
