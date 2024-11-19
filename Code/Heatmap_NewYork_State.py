import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt

# Load the newyork state health dataset
NYS_Data = pd.read_csv(r"C:\Users\Christoph\Documents\515\Project\Datasets\NewYorkState.csv")
NYC_Data = pd.read_csv(r"C:\Users\Christoph\Documents\515\Project\Datasets\NewYorkState.csv")

# Load the newyork state municipality data set I created 
municipality_to_county = pd.read_csv(r"C:\Users\Christoph\Downloads\county to municipality  - Sheet1.csv")

# Merge the dataframes on the 'municipality' column
merged_data = NYS_Data.merge(municipality_to_county, how='left', left_on='MUNICIPALITY', right_on='Municipality')

# Clean up column names (strip spaces, convert to lowercase for consistency)
merged_data['COUNTY'] = merged_data['COUNTY'].str.strip().str.lower()

# Convert 'TOTAL # CRITICAL VIOLATIONS' to numeric (this will coerce errors to NaN)
merged_data['TOTAL # CRITICAL VIOLATIONS'] = pd.to_numeric(merged_data['TOTAL # CRITICAL VIOLATIONS'], errors='coerce')

# Save the new dataset with counties
merged_data.to_csv('health_inspections_with_counties.csv', index=False)
print(merged_data.columns)

# Aggregate violations by county
by_county = merged_data.groupby('COUNTY', as_index=False)['TOTAL # CRITICAL VIOLATIONS'].sum()
print(by_county.head())

# Load NY State county shapefile (GeoJSON)
ny_counties = gpd.read_file(r"C:\Users\Christoph\Downloads\NYS_Civil_Boundaries_5848232821397896771.geojson")

# Normalize county names in the shapefile (lowercase and strip extra spaces)
ny_counties['county'] = ny_counties['NAME'].str.lower().str.strip()

# Merge aggregated data with GeoDataFrame by 'County'
ny_counties.rename(columns={'county': 'COUNTY'}, inplace=True)  # Make sure the column names match
by_county.rename(columns={'TOTAL # CRITICAL VIOLATIONS': 'critical_violations'}, inplace=True)

# Merge the data on 'County'
merged = ny_counties.merge(by_county, left_on='COUNTY', right_on='COUNTY', how='left')

# Print merged data to check
print(merged[['COUNTY', 'critical_violations']].head())

# Save the merged data
merged.to_csv('county_data_and_map_merged.csv', index=False)

# Plot the map of critical violations by county
plt.figure(figsize=(12, 10))
merged.plot(column='critical_violations', cmap='Reds', legend=True,
            legend_kwds={'label': "Critical Violations by County", 'orientation': "horizontal"},
            missing_kwds={"color": "lightgrey", "label": "No Data"},
            vmin=0, vmax=merged['critical_violations'].max())  # Set vmin and vmax
plt.show()