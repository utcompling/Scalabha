#!/usr/bin/python

#############################################################################
# Copyright 2011 Jason Baldridge
# 
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#############################################################################

# This script reads the format of the Debate 08 dataset obtained from:
#
#   http://www.infochimps.com/datasets/twitter-sentiment-dataset-2008-debates
#
# It produces an equal split of this data into thirds
# (train/dev/test), after filtering for tweets that have a two-thirds
# majority label that is positive, negative, or neutral. The data is
# ordered by time in the debate, such that the first third of the
# debate is train.xml, the second third is dev.xml, and the final
# third is test.xml.
#
# You MUST follow the license of the dataset:
#
#   Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License
# 
# See debate08_sentiment_tweets.tsv for further information, including
# citation information.

import sys,os
from operator import itemgetter
from xml.etree.ElementTree import Element, SubElement, tostring

def write_formatted_xml (tweets, file_name):
    output = file("tmp.xml",'w')
    output.write(tostring(create_dataset_xml(tweets)))
    output.close()
    os.system('xmllint --format ' + output.name + ' > ' + file_name)
    os.remove(output.name)


def create_dataset_xml (tweets):
    dataset = Element('dataset')
    for (tweetid,userid,username,label,target,content) in tweets:
        attributes = {"tweetid":tweetid, "username":username, "label":label, "target":target}
        itemEl = SubElement(dataset, "item", attributes)
        contentEl = SubElement(itemEl, "content")
        print content
        contentEl.text = unicode(content)
    return dataset


# To be included, 2 out of three votes must be for the majority label
vote_threshold = 2/3.0

annotations = [x for x in file(sys.argv[1]) if x[0].isdigit()]
filtered_tweets = []
for line in annotations:
    annotated_tweet = line.strip().split("\t")

    # Get the main information
    tweetid, time, content, name, nickname = annotated_tweet[:5]
    
    # Some nicknames are missing, in which case, use the name as nickname
    if nickname == "":
        nickname = name

    # Get the ratings
    ratings = annotated_tweet[5:]

    counts = {}
    [counts.__setitem__(label,1+counts.get(label,0)) for label in ratings] 
    countItems = counts.items()

    countItems.sort(key=itemgetter(1),reverse=True)
    bestLabel, countOfBest = countItems[0]
    
    # Python doesn't have a switch statement -- ARGH!!
    label = "mixed"
    if bestLabel == "1":
        label = "negative"
    elif bestLabel == "2":
        label = "positive"
    elif bestLabel == "4":
        label = "neutral" # "4" is "other", which we take as "neutral"

    # "3" is mixed, which we aren't handling here

    if label != "mixed" and countOfBest/float(len(ratings)) >= vote_threshold:
        filtered_tweets.append((time, tweetid, content, name, nickname, label))
    
filtered_tweets.sort()
tweets = []
userid = "000000" # we don't have userid's
for (time,tweetid,content,name,nickname,label) in filtered_tweets:

    # Attempt to identify the target as Obama, McCain, both, or general
    contentLower = content.lower()
    indexOfObama = contentLower.find("obama")
    indexOfMcCain = contentLower.find("mccain")
    target = "general"
    if indexOfObama > -1:
        target = "both" if indexOfMcCain > -1 else "obama"
    elif indexOfMcCain > -1:
        target = "mccain"

    tweets.append((tweetid,userid,name,label,target,content))

firstThirdIndex = int(round(len(tweets)/3.0))
secondThirdIndex = 2*firstThirdIndex

write_formatted_xml(tweets[:firstThirdIndex], "train.xml")
write_formatted_xml(tweets[firstThirdIndex:secondThirdIndex], "dev.xml")
write_formatted_xml(tweets[secondThirdIndex:], "test.xml")
