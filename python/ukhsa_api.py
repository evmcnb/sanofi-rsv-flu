import requests
import pandas as pd

def get_time_series_data(url: str) -> list[dict]:
    """
    Fetch time series data from a paginated API endpoint.

    Args:
        url (str): The initial URL to query.

    Returns:
        list[dict]: A list of records containing the data from all pages.
    """
    data = []

    while url:
        # Make an HTTP request to the current URL
        response = requests.get(url, params={"page_size": 365})

        # Check if the response was successful
        if not response.ok:
            raise Exception(f"Error, request failed with status code {response.status_code}: {response.text}")

        # Parse the response as JSON
        response_data = response.json()

        # Extract results and the next page URL
        results = response_data.get("results", [])
        next_url = response_data.get("next", None)

        # Append the results to the data list
        data.extend(results)

        # Update the URL for the next iteration (None if there's no next page)
        url = next_url

    return data


# Provided example for COVID
# url = "https://api.ukhsa-dashboard.data.gov.uk/themes/infectious_disease/sub_themes/respiratory/topics/COVID-19/geography_types/Nation/geographies/England/metrics/COVID-19_cases_casesByDay"

# Flu hosptial rates
# url = "https://api.ukhsa-dashboard.data.gov.uk/themes/infectious_disease/sub_themes/respiratory/topics/Influenza/geography_types/Nation/geographies/England/metrics/influenza_healthcare_hospitalAdmissionRateByWeek"

# RSV hospital rates
url = "https://api.ukhsa-dashboard.data.gov.uk/themes/infectious_disease/sub_themes/respiratory/topics/RSV/geography_types/Nation/geographies/England/metrics/RSV_healthcare_admissionRateByWeek"


# Fetch the data
try:
    data = get_time_series_data(url)

    # Convert the data to a Pandas DataFrame
    df = pd.DataFrame(data)

    name = 'RSV'

    # Save the DataFrame to a CSV file
    df.to_csv(f"{name}.csv", encoding="utf-8", index=False)
    print(f"Data successfully saved to '{name}.csv'.")
except Exception as e:
    print(f"An error occurred: {e}")
