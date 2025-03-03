import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import networkx as nx
from tqdm import tqdm

class COVIDPolicyABM:
    """
    Agent-based model for simulating COVID-19 policy interventions and their impacts
    on public health and socioeconomic outcomes.
    """
    
    def __init__(self, 
                 population_size=10000,
                 initial_infected=10,
                 infection_rate=0.3,
                 recovery_rate=0.1,
                 mortality_rate=0.02,
                 hospitalization_rate=0.15,
                 network_type="small-world",
                 economic_sectors=None,
                 policy_compliance_heterogeneity=True):
        """
        Initialize the model with parameters.
        
        Args:
            population_size: Number of agents in the simulation
            initial_infected: Number of initially infected agents
            infection_rate: Base probability of infection transmission
            recovery_rate: Probability of recovery per time step
            mortality_rate: Probability of death for infected agents
            hospitalization_rate: Probability of hospitalization for infected agents
            network_type: Type of social network ("random", "small-world", "scale-free")
            economic_sectors: Dictionary mapping sectors to proportion of population
            policy_compliance_heterogeneity: Whether to model heterogeneous compliance
        """
        self.population_size = population_size
        self.initial_infected = initial_infected
        self.infection_rate = infection_rate
        self.recovery_rate = recovery_rate
        self.mortality_rate = mortality_rate
        self.hospitalization_rate = hospitalization_rate
        self.network_type = network_type
        
        # default economic sectors
        if economic_sectors is None:
            self.economic_sectors = {
                "essential": 0.3,  # Healthcare, food, utilities
                "office": 0.4,     # WfM
                "service": 0.2,    # Hospitality, retail
                "manufacturing": 0.1  # Primary sector
            }
        else:
            self.economic_sectors = economic_sectors
            
        self.policy_compliance_heterogeneity = policy_compliance_heterogeneity
        
        # simulation state
        self.initialize_agents()
        self.initialize_network()
        self.history = {
            "susceptible": [],
            "infected": [],
            "recovered": [],
            "deceased": [],
            "hospitalized": [],
            "economic_impact": [],
            "income_inequality": [],
            "policy_approval": []
        }
        
    def initialize_agents(self):
        """Initialise agent population with attributes."""
        # 0=susceptible, 1=infected, 2=recovered, 3=deceased
        self.agents = np.zeros((self.population_size, 11), dtype=float)
        # Randomly assign ages (0-100)
        self.agents[:, 1] = np.random.normal(45, 18, self.population_size)
        self.agents[:, 1] = np.clip(self.agents[:, 1], 0, 100)
        
        # Assign income levels with Gini-like distribution
        self.agents[:, 2] = np.random.lognormal(mean=10, sigma=0.8, size=self.population_size)
        
        # Assign economic sectors
        sector_cutoffs = np.cumsum(list(self.economic_sectors.values()))
        sector_assignment = np.random.random(self.population_size)
        for i, sector in enumerate(self.economic_sectors.keys()):
            lower = 0 if i == 0 else sector_cutoffs[i-1]
            upper = sector_cutoffs[i]
            sector_mask = (sector_assignment >= lower) & (sector_assignment < upper)
            self.agents[sector_mask, 3] = i
            
        # Assign essential worker status (binary)
        # Essential workers in every sector, but concentrated in "essential" sector
        essential_prob = np.where(self.agents[:, 3] == 0, 0.8, 0.2)
        self.agents[:, 4] = np.random.random(self.population_size) < essential_prob
        
        # Assign wfm ability
        # Higher for office workers, lower for manufacturing and service
        wfh_probs = [0.3, 0.9, 0.2, 0.1]  # essential, office, service, manufacturing
        for i, prob in enumerate(wfh_probs):
            sector_mask = self.agents[:, 3] == i
            self.agents[sector_mask, 5] = np.random.random(np.sum(sector_mask)) < prob
            
        # health risk factors increases with age
        self.agents[:, 6] = 0.01 + 0.01 * (self.agents[:, 1] / 100) ** 2
        
        # baseline policy compliance (normal distribution)
        if self.policy_compliance_heterogeneity:
            self.agents[:, 7] = np.random.normal(0.7, 0.15, self.population_size)
            self.agents[:, 7] = np.clip(self.agents[:, 7], 0.1, 1.0)
        else:
            self.agents[:, 7] = 0.7
            
        # 0=not hospitalized
        self.agents[:, 8] = 0
        
        # Social connectivity (random assignment, will be replaced by network)
        self.agents[:, 9] = np.random.gamma(shape=2, scale=5, size=self.population_size)
        self.agents[:, 9] = np.clip(self.agents[:, 9], 1, 50)
        
        # Days infected (0 for all initially)
        self.agents[:, 10] = 0
        
        # Randomly infect initial agents
        initial_infected_idx = np.random.choice(self.population_size, self.initial_infected, replace=False)
        self.agents[initial_infected_idx, 0] = 1
        
    def initialize_network(self):
        """Create social network between agents based on specified topology."""
        if self.network_type == "random":
            # Erdos-Renyi random graph
            self.network = nx.erdos_renyi_graph(self.population_size, p=0.01)
        elif self.network_type == "small-world":
            # Watts-Strogatz small-world graph (more realistic for social networks)
            k = int(np.mean(self.agents[:, 9]))  # Average number of connections
            p_rewire = 0.1
            self.network = nx.watts_strogatz_graph(self.population_size, k, p_rewire)
        elif self.network_type == "scale-free":
            # Barabasi-Albert scale-free graph
            m = 5  # Number of edges for new node
            self.network = nx.barabasi_albert_graph(self.population_size, m)
        else:
            raise ValueError(f"Unknown network type: {self.network_type}")
        
        # Update agent social connectivity based on network degree
        degrees = dict(self.network.degree())
        for i in range(self.population_size):
            self.agents[i, 9] = degrees[i]
        
    def apply_policy(self, stringency, economic_support=0.5, messaging="neutral"):
        """
        Apply a policy with specified parameters.
        
        Args:
            stringency: 0-1 scale of how strict the policies are
            economic_support: 0-1 scale of government economic support
            messaging: "fear", "togetherness", "vulnerable", or "neutral"
        
        Returns:
            None - modifies agent behaviors based on policy
        """
        self.current_stringency = stringency
        self.current_economic_support = economic_support
        self.current_messaging = messaging
        
        # Adjust compliance based on messaging type
        messaging_modifier = {
            "fear": lambda: np.where(self.agents[:, 6] > 0.2, 0.8, -0.1),  # more compliance from high-risk, less from low-risk
            "togetherness": lambda: np.full(self.population_size, 0.1),  # moderate boost across population
            "vulnerable": lambda: np.where(self.agents[:, 6] > 0.15, 0.2, 0.05),  # stronger effect on all groups
            "neutral": lambda: np.zeros(self.population_size)
        }
            
        # Apply messaging effects to compliance
        modifier = messaging_modifier[self.current_messaging]()
        self.effective_compliance = self.agents[:, 7] + modifier
        self.effective_compliance = np.clip(self.effective_compliance, 0.1, 1.0)
        
        # economic impact by sector
        economic_impact_by_sector = {
            0: 0.1 * stringency,  # Essential - least affected
            1: 0.2 * stringency * (1 - np.mean(self.agents[self.agents[:, 3] == 1, 5])),  # Office - WFH reduces impact
            2: 0.8 * stringency,  # Service - heavily affected
            3: 0.6 * stringency  # Manufacturing - significantly affected
        }
        
        # economic impact to each agent, by sector
        self.economic_impact = np.zeros(self.population_size)
        for sector, impact in economic_impact_by_sector.items():
            sector_mask = self.agents[:, 3] == sector
            self.economic_impact[sector_mask] = impact * (1 - economic_support * 0.7)
            
        # essential workers have reduced economic impact but higher infection risk
        essential_mask = self.agents[:, 4] == 1
        self.economic_impact[essential_mask] *= 0.5
            
    def step(self):
        """Iterate one time step (day) in the model."""
        # agents current states
        current_infected = self.agents[:, 0] == 1
        current_susceptible = self.agents[:, 0] == 0
        
        # days infected for infected agents
        self.agents[current_infected, 10] += 1
        
        # process infections through social network
        new_infections = []
        
        for i in range(self.population_size):
            if current_susceptible[i]:
                # Check neighbors in social network
                neighbors = list(self.network.neighbors(i))
                infected_neighbors = [n for n in neighbors if current_infected[n]]
                
                # infection probability from neighbors
                base_infection_prob = self.infection_rate * len(infected_neighbors) / max(1, len(neighbors))
                
                # Modify probability based on compliance; higher compliance = lower contact rate
                infection_prob = base_infection_prob * (1 - self.effective_compliance[i] * self.current_stringency)
                
                # age increases risk
                age_factor = 1 + 0.01 * (self.agents[i, 1] / 100)
                infection_prob *= age_factor
                
                # essential workers have higher exposure
                if self.agents[i, 4] == 1:
                    infection_prob *= (2 - self.current_stringency * 0.5)
                
                # does infection occur?
                if np.random.random() < infection_prob:
                    new_infections.append(i)
        
        # apply new infections
        for i in new_infections:
            self.agents[i, 0] = 1
            self.agents[i, 10] = 0  # Reset days infected counter
            
        # process recoveries and deaths
        for i in np.where(current_infected)[0]:
            days_infected = self.agents[i, 10]
            
            # are they hospitalised?
            if self.agents[i, 8] == 0:  # yes
                hospital_prob = self.hospitalization_rate * self.agents[i, 6] * (1 + 0.1 * days_infected)
                if np.random.random() < hospital_prob:
                    self.agents[i, 8] = 1  # hospitalise
            
            # can they recover?
            recovery_modifier = 0.8 if self.agents[i, 8] == 1 else 1.0  # hospitalization improves recovery
            recovery_prob = self.recovery_rate * recovery_modifier * (days_infected / 14)  # More likely to recover after 14 days
            recovery_prob = min(recovery_prob, 0.5)  # hard cap on recovery probability
            
            # can they die?
            mortality_modifier = 0.4 if self.agents[i, 8] == 1 else 1.0 
            death_prob = self.mortality_rate * mortality_modifier * self.agents[i, 6] * (days_infected / 21)
            death_prob = min(death_prob, 0.1)  
            
            # outcomes 
            rand = np.random.random()
            if rand < death_prob:
                self.agents[i, 0] = 3  # deceased
                self.agents[i, 8] = 0  # remove from hospital
            elif rand < death_prob + recovery_prob:
                self.agents[i, 0] = 2  # recovered
                self.agents[i, 8] = 0 

        # update history
        self.history["susceptible"].append(np.sum(self.agents[:, 0] == 0))
        self.history["infected"].append(np.sum(self.agents[:, 0] == 1))
        self.history["recovered"].append(np.sum(self.agents[:, 0] == 2))
        self.history["deceased"].append(np.sum(self.agents[:, 0] == 3))
        self.history["hospitalized"].append(np.sum(self.agents[:, 8] == 1))
        
        # economic impact
        total_economic_impact = np.mean(self.economic_impact)
        self.history["economic_impact"].append(total_economic_impact)
        
        ## income inequality using Gini coefficient -> weighted income impact by initial income
        effective_income = self.agents[:, 2] * (1 - self.economic_impact)
        gini = self.calculate_gini(effective_income)
        self.history["income_inequality"].append(gini)
        
        # Calculate policy approval
        ## people approve if they're not infected and economic impact is low
        ## ...or if they recover from infection
        approval_factors = []
        
        ## health factors for approval
        health_approval = np.zeros(self.population_size)
        health_approval[self.agents[:, 0] == 0] = 0.7  # Susceptible moderately approve
        health_approval[self.agents[:, 0] == 1] = 0.3  # Infected less approve
        health_approval[self.agents[:, 0] == 2] = 0.8  # Recovered strongly approve
        
        ## economic factors for approval
        economic_approval = 1 - self.economic_impact
        
        ## approval weighted by personal priorities
        ## Higher risk individuals prioritise health, others economy
        health_weight = 0.7 * self.agents[:, 6] + 0.3
        health_weight = np.clip(health_weight, 0.3, 0.7)
        economic_weight = 1 - health_weight
        
        overall_approval = health_weight * health_approval + economic_weight * economic_approval
        self.history["policy_approval"].append(np.mean(overall_approval))
        
    def calculate_gini(self, values):
        """Calculate Gini coefficient for income inequality."""
        sorted_values = np.sort(values)
        n = len(values)
        cumsum = np.cumsum(sorted_values) # Gini numerator
        return (n + 1 - 2 * np.sum(cumsum) / (cumsum[-1])) / n # Gini formula
        
    def run_simulation(self, days, policy_changes=None):
        """
        Run simulation for specified number of days.
        
        Args:
            days: Number of days to simulate
            policy_changes: Day number to policy parameters
        """
        if policy_changes is None:
            policy_changes = {0: {"stringency": 0.5, "economic_support": 0.5, "messaging": "neutral"}}
            
        for day in tqdm(range(days)):
            # has policy changed?
            if day in policy_changes:
                self.apply_policy(**policy_changes[day])
            
            self.step()
            
            # if no more infections, terminate
            if self.history["infected"][-1] == 0:
                print(f"Epidemic ended on day {day}")
                break
                
        return self.get_results()
    
    def get_results(self):
        """Compile and return simulation results."""
        results = {
            "health_metrics": {
                "total_infected": self.population_size - self.history["susceptible"][-1],
                "percent_infected": 100 * (self.population_size - self.history["susceptible"][-1]) / self.population_size,
                "total_deceased": self.history["deceased"][-1],
                "mortality_rate": 100 * self.history["deceased"][-1] / self.population_size,
                "peak_hospitalization": max(self.history["hospitalized"]),
            },
            "economic_metrics": {
                "peak_economic_impact": max(self.history["economic_impact"]),
                "average_economic_impact": np.mean(self.history["economic_impact"]),
                "final_income_inequality": self.history["income_inequality"][-1],
                "inequality_change": self.history["income_inequality"][-1] - self.history["income_inequality"][0]
            },
            "social_metrics": {
                "final_policy_approval": self.history["policy_approval"][-1],
                "lowest_policy_approval": min(self.history["policy_approval"]),
                "approval_volatility": np.std(self.history["policy_approval"])
            },
            "time_series": self.history
        }
        return results
    
    def plot_results(self):
        """Generate visualisations of disease results."""
        fig = plt.figure(figsize=(18, 12))
        
        # (1): Disease progression
        ax1 = fig.add_subplot(2, 2, 1)
        ax1.plot(self.history["susceptible"], label="Susceptible", color="blue")
        ax1.plot(self.history["infected"], label="Infected", color="red")
        ax1.plot(self.history["recovered"], label="Recovered", color="green")
        ax1.plot(self.history["deceased"], label="Deceased", color="black")
        ax1.plot(self.history["hospitalized"], label="Hospitalized", color="purple")
        ax1.set_title("Disease Progression")
        ax1.set_xlabel("Days")
        ax1.set_ylabel("Population")
        ax1.legend()
        
        # (2) Economic impact
        ax2 = fig.add_subplot(2, 2, 2)
        ax2.plot(self.history["economic_impact"], label="Economic Impact", color="orange")
        ax2.plot(self.history["income_inequality"], label="Income Inequality (Gini)", color="brown")
        ax2.set_title("Economic Impact")
        ax2.set_xlabel("Days")
        ax2.set_ylabel("Impact (0-1)")
        ax2.legend()
        
        # (3) Approval
        ax3 = fig.add_subplot(2, 2, 3)
        ax3.plot(self.history["policy_approval"], label="Policy Approval", color="green")
        # stringency line, should be available but if not then ignore
        if hasattr(self, "current_stringency"):
            stringency_line = [self.current_stringency] * len(self.history["policy_approval"])
            ax3.plot(stringency_line, label="Policy Stringency", color="red", linestyle="--")
        ax3.set_title("Policy Approval")
        ax3.set_xlabel("Days")
        ax3.set_ylabel("Approval Rate (0-1)")
        ax3.legend()
        
        # (4): Sector-specific impacts
        ax4 = fig.add_subplot(2, 2, 4)
        sectors = ["Essential", "Office", "Service", "Manufacturing"]
        impacts = [np.mean(self.economic_impact[self.agents[:, 3] == i]) for i in range(4)]
        infected_rates = [np.sum(self.agents[self.agents[:, 3] == i, 0] == 1) / np.sum(self.agents[:, 3] == i) for i in range(4)]
        
        x = np.arange(len(sectors))
        width = 0.35
        ax4.bar(x - width/2, impacts, width, label="Economic Impact")
        ax4.bar(x + width/2, infected_rates, width, label="Infection Rate")
        ax4.set_title("Sector-Specific Impacts")
        ax4.set_xlabel("Sector")
        ax4.set_xticks(x)
        ax4.set_xticklabels(sectors)
        ax4.set_ylabel("Impact Rate (0-1)")
        ax4.legend()
        
        plt.tight_layout()
        return fig

###############################
def compare_policy_strategies():
    print("Comparing COVID-19 policy strategies using agent-based modeling...")
    
    # Define different policy interventions to compare
    policy_scenarios = {
        "No intervention": {
            0: {"stringency": 0.0, "economic_support": 0.0, "messaging": "neutral"}
        },
        "Strict lockdown": {
            0: {"stringency": 0.9, "economic_support": 0.5, "messaging": "fear"}
        },
        "Balanced approach": {
            0: {"stringency": 0.5, "economic_support": 0.7, "messaging": "togetherness"}
        },
        "Targeted protection": {
            0: {"stringency": 0.3, "economic_support": 0.6, "messaging": "vulnerable"}
        },
        "Adaptive strategy": {
            0: {"stringency": 0.3, "economic_support": 0.3, "messaging": "neutral"},
            30: {"stringency": 0.8, "economic_support": 0.8, "messaging": "togetherness"},
            60: {"stringency": 0.4, "economic_support": 0.6, "messaging": "vulnerable"}
        }
    }
    
    comparison_results = {}
    
    # Run each scenario
    for scenario_name, policy_changes in policy_scenarios.items():
        print(f"\nSimulating {scenario_name} scenario...")
        model = COVIDPolicyABM(population_size=5000, initial_infected=20)
        results = model.run_simulation(days=120, policy_changes=policy_changes)
        comparison_results[scenario_name] = results
        
        print(f"  Health outcomes:")
        print(f"    - Total infected: {results['health_metrics']['percent_infected']:.1f}%")
        print(f"    - Mortality rate: {results['health_metrics']['mortality_rate']:.2f}%")
        print(f"  Economic outcomes:")
        print(f"    - Economic impact: {results['economic_metrics']['average_economic_impact']:.2f}")
        print(f"    - Inequality change: {results['economic_metrics']['inequality_change']:.3f}")
        print(f"  Social outcomes:")
        print(f"    - Final approval: {results['social_metrics']['final_policy_approval']:.2f}")

    return comparison_results

if __name__ == "__main__":
    comparison_results = compare_policy_strategies()
