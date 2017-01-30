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

index_page = 'http://www.sportsbet.com.au/live-betting/'
page = urllib2.urlopen(index_page)

driver = webdriver.PhantomJS()
driver.get(index_page)

games = driver.find_elements_by_css_selector('#basketball-other .evt-label span,#basketball-us .evt-label span')

for game in games:
    print game.text
    
##Get the hrefs etc
    
Bsoup = BeautifulSoup(page,'lxml') 
soup_all_other = Bsoup.find_all('div', {'id': 'basketball-other'}) 
soup_all_us = Bsoup.find_all('div', {'id': 'basketball-us'})    

soup = soup_all_other[0] + soup_all_us[0]

soup0.find_all('td',{'class':'evt-label'})
    
    
    