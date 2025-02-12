import csv
import requests
import praw

from nrclex import NRCLex
from transformers import pipeline
from bs4 import BeautifulSoup
from scholarly import scholarly

keywords = ["antidepressant", "dialectic behavior therapy"]
matches = []

# set up reddit client

client_id = 'Ifj3vMDpZ8221X7IMt2VfQ'
client_secret = 'ZIlDTyouIc4B9t7mjJZ5DTreyK07-Q'
user_agent = 'KeywordSearchScript by /u/ImSoClassy'

reddit = praw.Reddit(client_id=client_id,
                     client_secret=client_secret,
                     user_agent=user_agent)

for keyword in keywords:
    # get reddit results
    for submission in reddit.subreddit("medicine").search(keyword, sort='new', time_filter='all', limit=10):
        matches.append(submission.title)

    # get google news results
    url = f"https://news.google.com/search?q={keyword}"
    search_results = requests.get(url)
    soup = BeautifulSoup(search_results.text, "html.parser")
    article_links = soup.find_all('a', class_='JtKRv')
    titles = []
    for link in article_links:
        title = link.get_text()
        titles.append(title)
    unique_titles = list(set(titles))[:10]
    for title in unique_titles:
        matches.append(title)

    # get google scholar results
    scholarly_search_results = scholarly.search_pubs(keyword)
    scholar_titles = []
    num_results = 10
    for i, result in enumerate(scholarly_search_results):
        if i > num_results:
            break
        scholar_titles.append(result['bib']['title'])
    for scholar_title in scholar_titles:
        matches.append(scholar_title)


def analyze_nrc_sentiment(text):
    emotion = NRCLex(text)
    return emotion.raw_emotion_scores


pipe = pipeline("sentiment-analysis", model="distilbert-base-uncased-finetuned-sst-2-english")

overall_sentiments = []
# complete BERT and NRC sentiment analysis
for text in matches:
    nrc_sentiment = analyze_nrc_sentiment(text)
    bert_sentiment = pipe(text)[0]
    sentiment_dict = {
        "BERT_label": bert_sentiment['label'],
        "BERT_score": bert_sentiment['score'],
        "nrc_sentiment": nrc_sentiment
    }
    overall_sentiments.append(sentiment_dict)

with open("testing.csv", "w", newline="", encoding="utf-8-sig") as file:
    writer = csv.writer(file)
    writer.writerow(["Title", 'Sentiment'])
    for match, sentiment in zip(matches, overall_sentiments):
        writer.writerow([match, sentiment])



