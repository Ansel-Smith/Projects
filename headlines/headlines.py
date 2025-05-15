# collects top 10 headlines from major news sources everyday for analysis of word use/length
#! python3

import requests, webbrowser, bs4, os, openpyxl
from datetime import date
from headlines_websites import websites
from headlines_functions import loader

os.chdir("/Users/super/Desktop/Projects/webscraping/headlines")

# read in headlines to txt files
for i in websites:
    os.makedirs(i, exist_ok=True)

    res = requests.get(websites[i]["link"])
    res.raise_for_status()

    soup = bs4.BeautifulSoup(res.text, 'html.parser')

    articles = soup.select(websites[i]["snip"])
    soup.prettify()
    file = open(os.path.join(i, os.path.basename(str(date.today())))+"_"+i+".txt",'w')

    for i in range(10):
        headline = articles[i].getText()
        headline = headline.strip()
        #headline = headline.encode('ascii', 'ignore')
        file.write(headline + '\n')

    file.close()

# upload strings from txt files to excel spreadsheet
for folder in websites:
    for file in os.listdir('./'+folder):
        loader(file)
        