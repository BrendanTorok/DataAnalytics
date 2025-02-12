import requests
import csv
import plotly.express as px
import pandas as pd
from bs4 import BeautifulSoup

keywords = ["freedom", "censor", "social media"]
articles = []
# Iterate through each keyword
for keyword in keywords:
    url = f"https://news.google.com/search?q={keyword}"
    search_results = requests.get(url)
    # create beautiful soup of search results to parse hmtl
    soup = BeautifulSoup(search_results.text, "html.parser")
    # parse html for article links and extract the title name from class
    article_links = soup.find_all('a', class_='JtKRv')
    # create empty titles list
    titles = []
    # iterate through each article link and get the title text and add it to titles list
    for link in article_links:
        title = link.get_text()
        titles.append(title)

    # limit the number of titles to 50 unique titles for each keyword
    unique_titles = list(set(titles))[:50]
    # add each unique title to the articles list
    for title in unique_titles:
        articles.append(title)

# save the articles list to a csv file, encode with utf-8-sig so excel csv saves and opens properly
with open("articles.csv", "w", newline="", encoding="utf-8-sig") as file:
    writer = csv.writer(file)
    writer.writerow(["title"])
    for title in articles:
        writer.writerow([title])


# open the csv file and count the occurrences of each keyword
keywords_count = {keyword: 0 for keyword in keywords}

with open("articles.csv", "r", newline="", encoding="utf-8-sig") as file:
    reader = csv.reader(file)
    # skip the title header
    next(reader)

    # iterate through each row
    for row in reader:
        title = row[0]
        # check each title for each keyword
        for keyword in keywords:
            # compare keyword to lower case title - comparison is case-insensitive
            # if it is found, increase count for keyword in dictionary
            if keyword in title.lower():
                keywords_count[keyword] += 1
print(keywords_count)

# convert the keywords_count dictionary to a dataframe
df = pd.DataFrame(list(keywords_count.items()), columns=['keywords', 'frequency'])

# plot the histogram
fig = px.histogram(df, x="keywords", y="frequency")
fig.update_layout(
    title_x=0.5,
    title_text='Frequency of Keywords in Titles From Google News Keyword Search',
    xaxis_title_text='Keywords',
    yaxis_title_text='Frequency'
)
fig.show()
