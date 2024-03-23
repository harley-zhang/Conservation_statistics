# Trinchera Ranch Summary Statistics

<div>
 <img src="https://img.shields.io/badge/R-%23276DC3.svg?style=for-the-badge&logo=R&logoColor=white"/>
 <br><br>
</div>
  
 Using R to summarize relevant 2022 and 2023 survey statistics for Trinchera Ranch, a conservation area in Fort Garland, Colorado. Developed with the Yale School of the Environment. Uses dplyr, tidyr, and stringr.

## Tree Statistics
- **Basal area per acre:** Calculates basal area in inches per acre for living trees.
- **Average diameter at breast height (DBH):** Calculates the average DBH in inches per acre for living trees.
- **Average height:** Calculates the average height in feet for living trees.
- **Dominant tree species:** Identifies the dominant tree species per plot, considering survey, frequency, and if the dominant one is less than or equal to 50% frequency, also determines the second-dominant tree species.

## Regeneration Statistics
- **Regeneration presence:** Checks if regeneration (saplings or seedlings) is present in each plot.
- **Seedlings per acre:** Calculates the number of seedlings per acre.

## Damage Statistics
- **Insect presence:** Determines if insect damage is present in each plot (Y/N).
- **Browse presence:** Identifies if browse damage is present in each plot (Y/N).
- **List of damage types:** Lists each type of damage present in each plot.
