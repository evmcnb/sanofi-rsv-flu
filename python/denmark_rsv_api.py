import requests
import urllib
import pandas as pd

# Mapping of Danish age groups to English equivalents
age_groups_translation = {
    "01. 0-5 måneder": "0-5 months",
    "02. 6-11 måneder": "6-11 months",
    "03. 1 år": "1 year",
    "04. 2 år": "2 years",
    "05. 3-5 år": "3-5 years",
    "06. 6-14 år": "6-14 years",
    "07. 15-44 år": "15-44 years",
    "08. 45-64 år": "45-64 years",
    "09. 65-74 år": "65-74 years",
    "10. 75-84 år": "75-84 years",
    "11. 85+ år": "85+ years"
}

# Use the Danish names for the query
age_groups = list(age_groups_translation.keys())

# Data storage
df = []

for age_grp in age_groups:
    print(f"Querying data for age group: {age_grp}")
    for value_type in ["cases", "tested", "admissions", "deaths", "incidens"]:
        q = urllib.parse.quote(f"(agegrp='{age_grp}')", safe="()")
        s = (
            "https://services5.arcgis.com/Hx7l9qUpAnKPyvNz/arcgis/rest/services/DB_rsvirus_data_gdb/FeatureServer/2/query?f=json&cacheHint=true&groupByFieldsForStatistics=week_sort%2CSeason&orderByFields=week_sort%20ASC&outFields=*&outStatistics=%5B%7B%22onStatisticField%22%3A%22"
            + value_type
            + "%22%2C%22outStatisticFieldName%22%3A%22value%22%2C%22statisticType%22%3A%22sum%22%7D%5D&resultType=standard&returnGeometry=false&spatialRel=esriSpatialRelIntersects&where=(region%3D%27Alle%27)%20AND%20"
            + q
            + "%20AND%20(gender%3D%27Alle%27)"
        )

        content = requests.get(s).json()
        for item in content["features"]:
            # Translate week_sort
            week_sort_raw = item['attributes']['week_sort']
            year_week = week_sort_raw.replace("År", "Year").replace("uge", "week").strip()

            d = {
                'week_sort': year_week,  # Translated
                'value': item['attributes']['value'],
                'type': value_type,
                'Season': item['attributes']['Season'],
                'Age Group': age_groups_translation[age_grp],  # Use English name
            }
            df.append(d)

# Convert to DataFrame
df = pd.DataFrame(df).set_index("week_sort")

# Save to CSV
df.to_csv("csv/Denmark/rsv_denmark.csv")

# Output DataFrame
print(df)

