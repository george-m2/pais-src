import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import numpy as np

VDEM_CSV_PATH = 'src/V-Dem-CY-Full+Others-v15.csv'
DEMOCRACY_INDEX_VAR = 'v2x_libdem'
DEM_THRESHOLD = 0.01
AUT_THRESHOLD = -0.01
START_YEAR = 1970
END_YEAR = 2022

print(f"Loading V-Dem data from: {VDEM_CSV_PATH}")
try:
    vdem_df = pd.read_csv(
        VDEM_CSV_PATH,
        usecols=['country_name', 'country_id', 'year', DEMOCRACY_INDEX_VAR]
    )
    print(f"Data loaded successfully. Shape: {vdem_df.shape}")
    print(f"Latest year in data: {vdem_df['year'].max()}")
    END_YEAR = min(END_YEAR, vdem_df['year'].max())
except FileNotFoundError:
    print(
        f"\n--- ERROR ---\n"
        f"V-Dem file not found at '{VDEM_CSV_PATH}'\n"
        "Please download the dataset and update VDEM_CSV_PATH.\n"
    )
    exit()
except ValueError as e:
    print(
        f"\n--- ERROR ---\n"
        f"Column error: {e}. Make sure '{DEMOCRACY_INDEX_VAR}' is present.\n"
    )
    exit()
except Exception as e:
    print(f"An error occurred: {e}")
    exit()

vdem_df = vdem_df[(vdem_df['year'] >= START_YEAR) & (vdem_df['year'] <= END_YEAR)].copy()
print(f"Data filtered for years {START_YEAR}-{END_YEAR}. Shape: {vdem_df.shape}")

vdem_df[DEMOCRACY_INDEX_VAR] = pd.to_numeric(vdem_df[DEMOCRACY_INDEX_VAR], errors='coerce')

global_mean_ldi = (
    vdem_df
    .dropna(subset=[DEMOCRACY_INDEX_VAR])
    .groupby('year')[DEMOCRACY_INDEX_VAR]
    .mean()
)
print("Global mean LDI calculated.")

vdem_df.sort_values(['country_id', 'year'], inplace=True)
vdem_df['ldi_change'] = vdem_df.groupby('country_id')[DEMOCRACY_INDEX_VAR].diff()

vdem_df_changes = vdem_df.dropna(subset=['ldi_change']).copy()
vdem_df_changes['is_democratizing'] = vdem_df_changes['ldi_change'] > DEM_THRESHOLD
vdem_df_changes['is_autocratizing'] = vdem_df_changes['ldi_change'] < AUT_THRESHOLD

transition_counts = vdem_df_changes.groupby('year')[['is_democratizing', 'is_autocratizing']].sum()
print("Transition counts calculated.")

all_years = pd.Index(range(START_YEAR, END_YEAR + 1), name='year')
plot_data = pd.DataFrame(index=all_years)
plot_data['mean_ldi'] = global_mean_ldi
plot_data['democratizing_count'] = transition_counts['is_democratizing'].fillna(0)
plot_data['autocratizing_count'] = transition_counts['is_autocratizing'].fillna(0)
plot_data['autocratizing_count_neg'] = -plot_data['autocratizing_count']
plot_data.dropna(subset=['mean_ldi'], inplace=True)

print("Data prepared for plotting.")
print(plot_data.tail())

fig, ax1 = plt.subplots(figsize=(15, 8))

color_ldi = 'tab:blue'
ax1.set_xlabel('Year', fontsize=14)
ax1.set_ylabel('Global Mean (LDI)', color=color_ldi, fontsize=14)
ax1.plot(plot_data.index, plot_data['mean_ldi'], color=color_ldi, linewidth=3.0, label='Mean LDI')
ax1.tick_params(axis='y', labelcolor=color_ldi, labelsize=12)
ax1.tick_params(axis='x', labelsize=12)
ax1.grid(axis='y', linestyle='--', alpha=0.7)

min_ldi, max_ldi = plot_data['mean_ldi'].min(), plot_data['mean_ldi'].max()
ax1.set_ylim(min_ldi * 0.98, max_ldi * 1.02)

ax2 = ax1.twinx()
ax2.set_ylabel('No. of countries (annual)', fontsize=14)
bar_width = 0.7

color_dem = 'forestgreen'
color_aut = 'firebrick'
ax2.bar(plot_data.index, plot_data['democratizing_count'], bar_width, color=color_dem, alpha=0.7, label='Democratizing countries')
ax2.bar(plot_data.index, plot_data['autocratizing_count_neg'], bar_width, color=color_aut, alpha=0.7, label='Autocratizing countries')
ax2.tick_params(axis='y', labelsize=12)

max_abs_count = max(plot_data['democratizing_count'].max(), plot_data['autocratizing_count'].max())
limit = np.ceil(max_abs_count * 1.1)
ax2.set_ylim(-limit, limit)
ax2.yaxis.set_major_locator(mticker.MaxNLocator(integer=True))
ax2.axhline(0, color='black', linewidth=1.0)

plt.title('Mean LDI relative to global regime transitions', fontsize=18, pad=25)

handles = [
    ax1.get_lines()[0],
    *ax2.containers
]
labels = ['Global Mean LDI', 'Democratizing', 'Autocratizing']
fig.legend(handles, labels, loc='upper center', bbox_to_anchor=(0.5, 0.04), ncol=3, fontsize=12)
fig.tight_layout(rect=[0, 0.08, 1, 0.94])

plt.savefig('src/ldi.png', dpi=300, bbox_inches='tight')
print("Plot saved to ldi.png.")