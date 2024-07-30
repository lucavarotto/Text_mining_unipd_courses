import requests
from bs4 import BeautifulSoup
import pandas as pd
import os

path = "C:/Users/Utente/OneDrive/Universita/2023-24/BigData/Progetto/"
os.chdir(path)

# URL of the website to scrape
url = "https://didattica.unipd.it/off/2021/LT"


url_radice = "https://didattica.unipd.it"

# Send an HTTP GET request to the website
response = requests.get(url)

# Parse the HTML code using BeautifulSoup
soup = BeautifulSoup(response.content, 'html.parser')

schools_list = [link.get("href") for link in soup.find_all('a')]
schools_url = list(filter(lambda item: item and item.startswith('/off/2021/LT/'), schools_list))

dat = []

for url in schools_url:
    
    url_full = url_radice+url

    response = requests.get(url_full)

    # Parse the HTML code using BeautifulSoup
    soup = BeautifulSoup(response.content, 'html.parser')

    school_name = soup.find(class_="list_scuole").find("td").get_text().strip()
    print(f"- {school_name}")

    degrees_list = [link.get("href") for link in soup.find_all('a')]
    degrees_url = list(filter(lambda item: item and item.startswith(url+"/"), degrees_list))

    for degree in degrees_url:

        degree = degree + "/000ZZ"

        url_full = url_radice+degree
        response = requests.get(url_full)

        # Parse the HTML code using BeautifulSoup
        soup = BeautifulSoup(response.content, 'html.parser')

        degree_name = soup.find_all(class_="selected")
        degree_name = degree_name[3].get_text().strip()
        print(f"----- CDL: {degree_name}")

        course_list = [link.get("href") for link in soup.find_all('a')]
        course_url = list(filter(lambda item: item and item.startswith(degree+"/"), course_list))
        
        for course in course_url:
            
            url_full = url_radice+course
            response = requests.get(url_full)

            # Parse the HTML code using BeautifulSoup
            soup = BeautifulSoup(response.content, 'html.parser')

            # approccio 1
            course_name = soup.find(class_="path").find_all("a")[-1].get_text()

            # approccio 2
            #course_name = soup.find(class_="titolopagina")

            # approccio 3
            #course_name = soup.find(class_="dettaglio")
            #course_name = course_name.find_all("td")[4].get_text().strip()
            print(f"---------- corso di: {course_name}")

            all_text = soup.find_all(class_="aleft vtop")
            len_all_text = len(all_text)
            if len_all_text <= 1:
                conoscenze_abilità = None
                contenuti = None
            elif len_all_text <= 4:
                conoscenze_abilità = all_text[1].get_text().strip()
                contenuti = None
            else:
                conoscenze_abilità = all_text[1].get_text().strip()
                contenuti = all_text[4].get_text().strip()

            dat.append([school_name, degree_name, course_name, conoscenze_abilità, contenuti, url_full])

print("### DONE! ###")

# Store the information in a pandas dataframe
df = pd.DataFrame(dat, columns=['School', 'Degree', "Course", "Conoscenze e abilita' da acquisire", "Contenuti", "URL"])

# Save the dataframe to a CSV file
df.to_csv('unipd_courses.csv', index=False)