import time

start_time = time.time()
import os
import json
import pandas as pd
import numpy as np
import pymysql
import sqlalchemy
import spotipy
import spotipy.util as util
import re
from datetime import datetime
import pprint

with open(r'File_Location', encoding = 'utf8') as j:
    playlist_dict = json.loads(j.read())
streaming_0 = pd.read_json(r'File_Location')
streaming_1 = pd.read_json(r'File_Location')
streaming = pd.concat([streaming_0, streaming_1], axis = 0, join = "outer")
streaming["combinedArtistSong"] = streaming["trackName"] + " - " + streaming["artistName"]
streaming = streaming.reset_index(drop=True)

#Rearranging playlist dictionary

playlist_names = []
for playlist in range(len(playlist_dict['playlists'])):
    playlist_names.append(playlist_dict['playlists'][playlist]['name'])
playlist_names = pd.DataFrame(playlist_names)

playlist_songs = pd.DataFrame([])
for playlist in range(len(playlist_dict['playlists'])):
    try:
        track_list_i = playlist_dict['playlists'][playlist].get('items')
        track_list_i = pd.DataFrame(track_list_i)['track']
        track_list_i = pd.DataFrame(track_list_i.tolist())
        track_list_i["Playlist"] = playlist_names.iloc[playlist]
        track_list_i["Playlist"] = track_list_i["Playlist"].fillna(method="ffill")
        playlist_songs = pd.concat([playlist_songs, track_list_i], axis=0, join='outer')
    except:
        pass

playlist_songs = playlist_songs.drop(playlist_songs.columns[[5]], axis = 1)
playlist_songs = playlist_songs.dropna()
playlist_songs = playlist_songs.reset_index(drop=True)

#Authentication
client_id = 'XXXXXXX'
client_secret = 'XXXXXXX'
username = "griffin_sleigh"
scope = "user-read-currently-playing user-read-recently-played"
redirect_uri = "http://localhost:8888/callback/"

token = util.prompt_for_user_token(username, scope, client_id = client_id, client_secret = client_secret, redirect_uri = redirect_uri)
sp = spotipy.Spotify(auth=token)

uri_df = pd.DataFrame([])
for track in range(len(playlist_songs)):
    try:
        uri = playlist_songs.at[track, 'trackUri']
        track_info = pd.DataFrame(sp.audio_features(uri)[0], index = [track])
        track_info['track_image'] = sp.track(uri, market = 'AU')['album']['images'][0]['url']
        track_info['track_url'] = sp.track(uri, market = 'AU')['external_urls']['spotify']
        uri_df = pd.concat([uri_df, track_info], axis = 0)
    except:
        token = util.prompt_for_user_token(username, scope, client_id=client_id, client_secret=client_secret,redirect_uri=redirect_uri)
        sp = spotipy.Spotify(auth=token)
        uri = playlist_songs.at[track, 'trackUri']
        track_info = pd.DataFrame(sp.audio_features(uri)[0], index = [track])
        track_info['track_image'] = sp.track(uri, market = 'AU')['album']['images'][0]['url']
        track_info['track_url'] = sp.track(uri, market = 'AU')['external_urls']['spotify']
        uri_df = pd.concat([uri_df, track_info], axis = 0)

playlist_songs = pd.concat([playlist_songs, uri_df], axis = 1)
playlist_songs["combinedArtistSong"] = playlist_songs["trackName"] + " - " + playlist_songs["artistName"]
playlist_songs['countPlayed'] = ""

#Adding some elements to playlist_songs from streaming
last_date = datetime.strptime(max(streaming["endTime"]), "%Y-%m-%d %H:%M")

for song in range(len(playlist_songs)):
    comb_artist_song = re.escape(playlist_songs.at[song, "combinedArtistSong"])
    playlist_songs.at[song, 'countPlayed'] = streaming['combinedArtistSong'].str.count(comb_artist_song).sum()
    first_played_row = streaming['combinedArtistSong'].str.contains(comb_artist_song).idxmax()
    first_played_date = datetime.strptime(streaming.at[first_played_row, "endTime"], "%Y-%m-%d %H:%M")
    playlist_songs.at[song, 'daysSinceFirstPlayed'] = (last_date - first_played_date).days

#transpose playlist_songs for spider chart
playlist_songs_transposing = playlist_songs[["combinedArtistSong", "acousticness", "danceability", "energy", "liveness", "loudness", "speechiness", "valence"]]

streaming_size = pd.DataFrame(streaming.groupby("combinedArtistSong").size(), columns = ["plays"])
streaming_first_dates = pd.DataFrame(streaming.groupby("combinedArtistSong").first())
streaming_first_dates["endTime"] = streaming_first_dates["endTime"].apply(lambda x:datetime.strptime(x,"%Y-%m-%d %H:%M"))
streaming_grouped = streaming_size.join(streaming_first_dates)
streaming_grouped["daysSinceFirstPlay"] = (last_date - streaming_grouped['endTime']).astype('timedelta64[D]')
streaming_grouped["addedToEverything"] = ""
streaming_grouped["combinedArtistSong"] = streaming_grouped.index
streaming_grouped = streaming_grouped.reset_index(drop=True)

start_time = time.time()
token = util.prompt_for_user_token(username, scope, client_id = client_id, client_secret = client_secret, redirect_uri = redirect_uri)
sp = spotipy.Spotify(auth=token)

uri_df = pd.DataFrame([])
for song in range(len(streaming_grouped)):
    try:
        track_info = pd.DataFrame([])
        comb_artist_song = streaming_grouped.at[song, "combinedArtistSong"]
        artist = streaming_grouped.at[song, "artistName"]
        track = streaming_grouped.at[song, "trackName"]
        if playlist_songs["combinedArtistSong"].str.contains(re.escape(comb_artist_song)).any():
            streaming_grouped.at[song, "addedToEverything"] = 1
        else:
            streaming_grouped.at[song, "addedToEverything"] = 0
        searchResults = sp.search(q="artist:" + artist + " track:" + track, type="track", market="AU")
        try:
            uri = searchResults['tracks']['items'][0].get('uri')
            track_info = pd.DataFrame(sp.audio_features(uri)[0], index=[song])
            track_info['track_image'] = sp.track(uri, market='AU')['album']['images'][0]['url']
            track_info['track_url'] = sp.track(uri, market='AU')['external_urls']['spotify']
        except:
            uri = "NaN"
            track_info = pd.DataFrame(sp.audio_features(uri)[0], index=[song])
        uri_df = pd.concat([uri_df, track_info], axis = 0)
    except:
        token = util.prompt_for_user_token(username, scope, client_id=client_id, client_secret=client_secret, redirect_uri=redirect_uri)
        sp = spotipy.Spotify(auth=token)
        track_info = pd.DataFrame([])
        comb_artist_song = streaming_grouped.at[song, "combinedArtistSong"]
        artist = streaming_grouped.at[song, "artistName"]
        track = streaming_grouped.at[song, "trackName"]
        if playlist_songs["combinedArtistSong"].str.contains(re.escape(comb_artist_song)).any():
            streaming_grouped.at[song, "addedToEverything"] = 1
        else:
            streaming_grouped.at[song, "addedToEverything"] = 0
        searchResults = sp.search(q="artist:" + artist + " track:" + track, type="track", market="AU")
        try:
            uri = searchResults['tracks']['items'][0].get('uri')
            track_info = pd.DataFrame(sp.audio_features(uri)[0], index=[song])
            track_info['track_image'] = sp.track(uri, market='AU')['album']['images'][0]['url']
            track_info['track_url'] = sp.track(uri, market='AU')['external_urls']['spotify']
        except:
            uri = "NaN"
            track_info = pd.DataFrame(sp.audio_features(uri)[0], index=[song])
        uri_df = pd.concat([uri_df, track_info], axis = 0)

streaming_grouped = pd.concat([streaming_grouped, uri_df], axis = 1)
streaming_grouped = streaming_grouped.dropna()

user_info = sp.user(username)
username = user_info['display_name']
user_url = user_info['external_urls']['spotify']

misc_info = {"username": username, "user_url": user_url}
misc_info = pd.DataFrame([misc_info])

#TODO Power BI add current song playing
import spotipy
import spotipy.util as util

#Authentication
client_id = 'XXXXXXX'
client_secret = 'XXXXXXX'
username = "griffin_sleigh"
scope = "user-read-currently-playing user-read-recently-played"
redirect_uri = "http://localhost:8888/callback/"

token = util.prompt_for_user_token(username, scope, client_id = client_id, client_secret = client_secret, redirect_uri = redirect_uri)
sp = spotipy.Spotify(auth=token)

try:
    currently_playing = sp.currently_playing()
    misc_info["currently_playing_uri"] = currently_playing["item"]["uri"]
    misc_info["currently_playing_image"] = currently_playing["item"]["album"]["images"][0]["url"]
    misc_info["currently_playing_artist"] = currently_playing["item"]["artists"][0]["name"]
    misc_info["currently_playing_song"] = currently_playing["item"]["name"]
    misc_info["currently_playing_song_link"] = currently_playing["item"]["external_urls"]["spotify"]
    misc_info["play_status"] = "Currently Playing"
except:
    currently_playing = sp.current_user_recently_played(limit=1)
    misc_info["currently_playing_uri"] = currently_playing['items'][0]['track']["uri"]
    misc_info["currently_playing_image"] = currently_playing['items'][0]['track']["album"]["images"][0]["url"]
    misc_info["currently_playing_artist"] = currently_playing['items'][0]['track']["artists"][0]["name"]
    misc_info["currently_playing_song"] = currently_playing['items'][0]['track']["name"]
    misc_info["currently_playing_song_link"] = currently_playing['items'][0]['track']["external_urls"]["spotify"]
    misc_info["play_status"] = "Last Played"


#Connecting to MySQL to export
sqlEngine = sqlalchemy.create_engine('XXXXXXX')
dbConnection = sqlEngine.connect()
frame = streaming_grouped.to_sql("streaming", dbConnection, if_exists='replace', index=False)
frame_2 = playlist_songs.to_sql("playlist_songs", dbConnection, if_exists='replace', index=False)
frame_3 = misc_info.to_sql("misc_info", dbConnection, if_exists='replace', index=False)

print((time.time() - start_time)/60)

#Import from SQL

import mysql.connector as connection
try:
    mydb = connection.connect(host="XXXXXXX", database = 'XXXXXXX',user="XXXXXXX", passwd="XXXXXXX",use_pure=True)
    query = "Select * from streaming;"
    streaming_grouped = pd.read_sql(query,mydb)
    query = "Select * from playlist_songs;"
    playlist_songs = pd.read_sql(query,mydb)
    mydb.close() #close the connection
except Exception as e:
    mydb.close()
    print(str(e))
    
#PREDICTION FOR WITHIN THE DASHBOARD

from sklearn import preprocessing
import matplotlib.pyplot as plt
import statsmodels.api as sm
from sklearn.tree import DecisionTreeClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn import tree

#DATASETS
y = pd.DataFrame(streaming_grouped['addedToEverything'])
y = y.astype('int')
x = streaming_grouped[['tempo', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'danceability', 'energy']]
feature_cols = ['tempo', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'danceability', 'energy']
class_names = ['Not', 'Added']
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.3, random_state=1) # 70% training and 30% test

#DECISION TREE
clf = DecisionTreeClassifier(criterion='entropy', max_depth=10)
clf = clf.fit(x_train, y_train)
y_pred = clf.predict(x_test)
print("Accuracy:", metrics.accuracy_score(y_test, y_pred))
cnf_matrix_dt = metrics.confusion_matrix(y_test, y_pred)
# Setting dpi = 300 to make image clearer than default
fig, axes = plt.subplots(nrows = 1,ncols = 1,figsize = (4,4), dpi=600)

tree.plot_tree(clf,
           feature_names = feature_cols,
           class_names= class_names,
           filled = True);

fig.savefig('imagename.png')

#LOGISTIC REGRESSION - CHANGE X BUT KEEP Y THE SAME
from statsmodels.stats.outliers_influence import variance_inflation_factor
# VIF dataframe
vif_data = pd.DataFrame()
vif_data["feature"] = x.columns
# calculating VIF for each feature
vif_data["VIF"] = [variance_inflation_factor(x.values, i)
                   for i in range(len(x.columns))]


logreg = LogisticRegression(fit_intercept=True)
logreg.fit(x_train,y_train)
y_pred=logreg.predict(x_test)
cnf_matrix = metrics.confusion_matrix(y_test, y_pred)
logsm = sm.Logit(y_train, x_train).fit()
print("Accuracy:",metrics.accuracy_score(y_test, y_pred))

#K-NEAREST NEIGHBOURS
knn = KNeighborsClassifier(n_neighbors=25)
knn.fit(x_train, y_train)
y_pred = knn.predict(x_test)
print("Accuracy:", metrics.accuracy_score(y_test, y_pred))

