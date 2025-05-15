import requests, webbrowser, bs4, os
from datetime import date
from headlines_websites import websites
from headlines_functions import loader
import openpyxl

os.chdir("/Users/super/Desktop/Projects/webscraping/headlines")

for folder in websites:
    for file in os.listdir('./'+folder):
        loader(file)
        