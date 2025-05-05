import os
import re
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

TITLE_FONTSIZE = 18
AXIS_LABEL_FONTSIZE = 14
TICK_LABEL_FONTSIZE = 12
LEGEND_FONTSIZE = 12
LINE_WIDTH = 3.0

def plot_cpi_data(csv_path):
    """
    Reads a CPI CSV, computes global and regional averages, and saves time-series plots.
    """
    if not os.path.exists(csv_path):
        print(f"Error: File not found at {csv_path}")
        return

    try:
        df = pd.read_csv(csv_path, header=3)
        df.columns = df.columns.str.strip()

        score_cols = {}
        for col in df.columns:
            m = re.search(r'CPI (?:S|s)core (\d{4})', col)
            if m:
                score_cols[col] = int(m.group(1))

        if not score_cols or 'Region' not in df.columns:
            print("Error: Missing 'Region' or CPI score columns.")
            return

        df = df[['Region'] + list(score_cols)].rename(columns=score_cols)
        for year in score_cols.values():
            df[year] = pd.to_numeric(df[year], errors='coerce')

        years = sorted(score_cols.values())
        global_avg = df[years].mean()
        regional_avg_T = df.groupby('Region')[years].mean().T

        sns.set_theme(style="whitegrid")

        # Global average plot
        plt.figure(figsize=(12, 6))
        sns.lineplot(global_avg.index, global_avg.values,
                     marker='o', label='Global', linewidth=LINE_WIDTH)
        plt.title('Global Average CPI Score', fontsize=TITLE_FONTSIZE)
        plt.xlabel('Year', fontsize=AXIS_LABEL_FONTSIZE)
        plt.ylabel('Score', fontsize=AXIS_LABEL_FONTSIZE)
        mn, mx = global_avg.min(), global_avg.max()
        pad = (mx - mn) * 0.1
        plt.ylim(max(0, mn - pad), min(100, mx + pad))
        plt.xticks(years, rotation=45, fontsize=TICK_LABEL_FONTSIZE)
        plt.yticks(fontsize=TICK_LABEL_FONTSIZE)
        plt.legend(fontsize=LEGEND_FONTSIZE)
        plt.tight_layout()
        os.makedirs("img", exist_ok=True)
        plt.savefig("img/cpi_global_average_timeseries.png")
        plt.show()

        # Regional averages plot
        plt.figure(figsize=(16, 9))
        sns.set_style("whitegrid")
        plt.rcParams.update({
            'axes.edgecolor': 'lightgrey',
            'grid.linestyle': '--',
            'grid.color': 'lightgrey'
        })
        palette = sns.color_palette("tab10", len(regional_avg_T.columns))
        for i, region in enumerate(regional_avg_T.columns):
            sns.lineplot(regional_avg_T.index, regional_avg_T[region],
                         label=region, color=palette[i], linewidth=LINE_WIDTH)

        plt.title('Average CPI Score by Region', fontsize=TITLE_FONTSIZE)
        plt.xlabel('Year', fontsize=AXIS_LABEL_FONTSIZE)
        plt.ylabel('Score', fontsize=AXIS_LABEL_FONTSIZE)
        mn2, mx2 = regional_avg_T.min().min(), regional_avg_T.max().max()
        pad2 = (mx2 - mn2) * 0.05
        plt.ylim(max(0, mn2 - pad2), min(100, mx2 + pad2))
        plt.xticks(years, rotation=45, ha='right', fontsize=TICK_LABEL_FONTSIZE)
        plt.yticks(fontsize=TICK_LABEL_FONTSIZE)
        plt.legend(
            title='Region', loc='lower center', bbox_to_anchor=(0.5, -0.25),
            ncol=min(len(regional_avg_T.columns), 5),
            frameon=False, fontsize=LEGEND_FONTSIZE
        )
        plt.tight_layout(rect=[0, 0.05, 1, 1])
        plt.savefig("img/cpi_regional_average_timeseries.png", bbox_inches='tight')
        plt.show()

    except Exception as e:
        print(f"An unexpected error occurred: {e}")

if __name__ == "__main__":
    csv_file = "src/CPI2023_GlobalResultsTrends.csv"
    plot_cpi_data(csv_file)


