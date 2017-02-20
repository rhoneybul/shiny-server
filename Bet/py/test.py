from bs4 import BeautifulSoup
import urllib
from selenium import webdriver
import time
import re

driver = webdriver.Firefox()

driver.get('https://www.bet365.com.au')

driver.find_element_by_id('TopPromotionBetNow').click()
time.sleep(5)
curr_url = driver.current_url

live_url = re.sub('#/HO/','#/IP/',curr_url)

print live_url 
driver.get(live_url)

time.sleep(5)

driver.find_elements_by_css_selector('.ipc-InPlayClassificationIcon-18').click()
