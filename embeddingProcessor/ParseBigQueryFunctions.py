from bs4 import BeautifulSoup
import urllib.request
import re

request = urllib.request.Request('https://cloud.google.com/bigquery/docs/reference/standard-sql/functions-and-operators')
response = urllib.request.urlopen(request)
page = response.read()

soup = BeautifulSoup(page, 'html.parser')

h = soup.find("h2")
for s in h.find_next_siblings("pre"):
    txt = s.get_text().lower()
    txt = re.sub('[(][^)]*[)]',' ', txt)

    if 'select' in txt:
        continue
    try:
        description = s.find_next_sibling('p', string="Description").find_next_sibling('p')
        print(txt + " " + description.get_text().replace("\n", ""))
    except:
        pass

