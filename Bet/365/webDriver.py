import urllib,json
import sqlite3
import re
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from string import digits
import re
import numpy as np
import time
import sqlite3
import os
import sys
import urllib2
from bs4 import BeautifulSoup

def rem_from_dir():
    files = os.listdir(os.getcwd())
    for file in files:
        os.remove(file)

os.chdir('/Users/roberttimothyhoneybul/Desktop/RTH/Clients/365/')

index_page = 'http://www.sportsbet.com.au/live-betting/'
page = urllib2.urlopen(index_page)

driver = webdriver.PhantomJS()
driver.get(index_page)

table = driver.find_elements_by_css_selector('td')

all_tab = []
for tab in table:
    all_tab.append(tab.text.encode('ascii','ignore'))

tab_text = ','.join(all_tab)
tab_text = tab_text.split(",,,")

for tab in tab_text:
    tab = re.sub('^\\,\\,','',tab)

os.chdir('Index')
rem_from_dir()
file = urllib.URLopener()
file.retrieve(index_page,'index.html')
os.chdir('..')

Bsoup = BeautifulSoup(page,'lxml') 

soup_all = Bsoup.find_all('td', {'class' : 'evt-label'})

os.chdir('Games')
rem_from_dir()
game_list = []
for soup in soup_all:
    if len(re.sub("\n","",soup.text).encode('ascii','ignore')) > 100:
        break
    print re.sub("\n","",soup.text).encode('ascii','ignore')
    game_list.append(re.sub("\n","",soup.text).encode('ascii','ignore'))
    file = urllib.URLopener()
    file.retrieve(soup.find('a')['href'],re.sub("\n","",soup.text).encode('ascii','ignore') + '.html') 

indices = []
for game in game_list:
    for ii in range(len(tab_text)):
        if game in tab_text[ii]:
            tab_ii = tab_text.split(',')
            tab_ii = tab_ii[:tab_ii.index(game)]
            
            
            