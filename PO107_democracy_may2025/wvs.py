import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import numpy as np

# --- Configuration ---
CSV_FILE_PATH = 'wvs.csv'   # Path to your WVS CSV file

INSTITUTION_VARS = {
    'Parliament': 'E069_18',
    'Government': 'E069_20',
}

YEAR_VAR = 'S020'
COUNTRY_VAR = 'COUNTRY_ALPHA'
WEIGHT_VAR = 'S017'

EU_COUNTRY_CODES = [
    'AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA',
    'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'MLT', 'NLD',
    'POL', 'PRT', 'ROU', 'SVK', 'SVN', 'ESP', 'SWE'
]
TARGET_COUNTRIES = EU_COUNTRY_CODES

LOW_TRUST_VALUES = [3, 4]
VALID_TRUST_VALUES = [1, 2, 3, 4]

# Map survey year to WVS wave label
wave_map = [
    ((1981, 1984), 'W1 (81–84)'),
    ((1990, 1994), 'W2 (90–94)'),
    ((1995, 1998), 'W3 (95–98)'),
    ((1999, 2004), 'W4 (99–04)'),
    ((2005, 2009), 'W5 (05–09)'),
    ((2010, 2014), 'W6 (10–14)'),
    ((2017, 2022), 'W7 (17–22)'),
]

def get_wvs_wave(year):
    for (start, end), label in wave_map:
        if start <= year <= end:
            return label
    return None

# --- Load Data ---
print(f"Loading data from: {CSV_FILE_PATH}")
cols = list(INSTITUTION_VARS.values()) + [YEAR_VAR, COUNTRY_VAR, WEIGHT_VAR]
try:
    df = pd.read_csv(CSV_FILE_PATH, usecols=cols, low_memory=False)
except Exception as e:
    print(f"Error loading data: {e}")
    exit()
print(f"Total rows: {len(df)}")

# Filter for EU countries
if TARGET_COUNTRIES:
    df = df[df[COUNTRY_VAR].isin(TARGET_COUNTRIES)]
    print(f"Rows after EU filter: {len(df)}")
    if df.empty:
        print("No data for specified EU countries."); exit()

# Convert columns to numeric and drop missing
for col in [YEAR_VAR, WEIGHT_VAR] + list(INSTITUTION_VARS.values()):
    df[col] = pd.to_numeric(df[col], errors='coerce')
df.dropna(subset=[YEAR_VAR, WEIGHT_VAR], inplace=True)
df[YEAR_VAR] = df[YEAR_VAR].astype(int)
print(f"Rows after dropping missing year/weight: {len(df)}")

# Assign waves and drop unknown
df['Wave'] = df[YEAR_VAR].apply(get_wvs_wave)
df.dropna(subset=['Wave'], inplace=True)
# Exclude early waves if needed
to_exclude = ['W1 (81–84)', 'W2 (90–94)']
df = df[~df['Wave'].isin(to_exclude)]
print(f"Rows after wave filtering: {len(df)}")

# Ensure at least two waves
waves = sorted(df['Wave'].unique())
if len(waves) < 2:
    print("Not enough waves for analysis."); exit()
print(f"Analyzing waves: {waves}")

# Compute weighted low-trust percentage per wave
def weighted_low_trust_pct(gr, var, wt):
    sub = gr.dropna(subset=[var, wt])
    sub = sub[sub[var].isin(VALID_TRUST_VALUES)]
    w = sub[wt].sum()
    if w == 0:
        return np.nan
    low = sub[var].isin(LOW_TRUST_VALUES).astype(int)
    return (low * sub[wt]).sum() / w * 100

results = {}
for name, var in INSTITUTION_VARS.items():
    s = df.groupby('Wave').apply(lambda grp: weighted_low_trust_pct(grp, var, WEIGHT_VAR))
    results[name] = s

results_df = pd.DataFrame(results).reindex(waves)
results_df.dropna(how='all', inplace=True)
if results_df.shape[0] < 2:
    print("Insufficient data points."); exit()

print("Computed low-trust percentages:")
print(results_df)

# --- Plotting ---
fig, ax = plt.subplots(figsize=(15, 8))

# Style parameters
TITLE_FS = 18
AX_LBL_FS = 14
TICK_FS = 12
LEG_FS = 12
LW = 3.0

for col in results_df.columns:
    ax.plot(results_df.index, results_df[col], linewidth=LW, label=col)

ax.grid(axis='y', linestyle='--', alpha=0.7)
ax.set_axisbelow(True)

ax.set_xlabel('WVS Wave', fontsize=AX_LBL_FS)
ax.set_ylabel('Low-Trust (%)', fontsize=AX_LBL_FS)
ax.set_title('Public trust in Government and Parliament (WVS waves, 1995-2022)', fontsize=TITLE_FS, pad=25)

# X-ticks and rotation alignment
ax.tick_params(axis='x', labelsize=TICK_FS, rotation=25)
ax.tick_params(axis='y', labelsize=TICK_FS)

# Legend below plot
handles, labels = ax.get_legend_handles_labels()
fig.legend(handles, labels,
           loc='upper center', bbox_to_anchor=(0.5, 0.04),
           ncol=len(labels), fontsize=LEG_FS)

fig.tight_layout(rect=[0, 0.08, 1, 0.95])
output_file = 'wvs_low_trust_trend.png'
plt.savefig(output_file, dpi=300, bbox_inches='tight')
plt.show()
print(f"Plot saved as '{output_file}'.")
