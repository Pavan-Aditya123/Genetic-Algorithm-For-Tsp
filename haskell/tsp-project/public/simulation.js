/**
 * TSP Simulation Module
 * Provides tools for generating random TSP problems and analyzing algorithm performance
 */

class TSPSimulation {
    constructor() {
        this.cities = [];
        this.bestKnownSolutions = new Map(); // For benchmark problems with known solutions
        this.simulationResults = [];
        this.currentSimulation = null;
    }
    
    /**
     * Generate a random TSP problem with given parameters
     * @param {number} numCities - Number of cities to generate
     * @param {string} distributionType - Type of distribution ('uniform', 'cluster', 'circle')
     * @param {number} width - Width of the area
     * @param {number} height - Height of the area
     * @returns {Array} - Array of city objects
     */
    generateRandomProblem(numCities, distributionType = 'uniform', width = 800, height = 600) {
        // Clear existing cities
        this.cities = [];
        
        switch (distributionType) {
            case 'uniform':
                this.generateUniformCities(numCities, width, height);
                break;
            case 'cluster':
                this.generateClusteredCities(numCities, width, height);
                break;
            case 'circle':
                this.generateCircularCities(numCities, width, height);
                break;
            default:
                this.generateUniformCities(numCities, width, height);
        }
        
        return this.cities;
    }
    
    /**
     * Generate uniformly distributed cities
     */
    generateUniformCities(numCities, width, height) {
        for (let i = 0; i < numCities; i++) {
            this.cities.push({
                name: `City ${i+1}`,
                x: Math.floor(Math.random() * width),
                y: Math.floor(Math.random() * height)
            });
        }
    }
    
    /**
     * Generate cities in clusters
     */
    generateClusteredCities(numCities, width, height) {
        // Generate cluster centers
        const numClusters = Math.min(5, Math.ceil(numCities / 10));
        const clusters = [];
        
        for (let i = 0; i < numClusters; i++) {
            clusters.push({
                x: Math.floor(Math.random() * width),
                y: Math.floor(Math.random() * height),
                radius: 50 + Math.floor(Math.random() * 100)
            });
        }
        
        // Generate cities around clusters
        for (let i = 0; i < numCities; i++) {
            const clusterIndex = i % numClusters;
            const cluster = clusters[clusterIndex];
            const angle = Math.random() * 2 * Math.PI;
            const distance = Math.random() * cluster.radius;
            
            const x = Math.floor(cluster.x + distance * Math.cos(angle));
            const y = Math.floor(cluster.y + distance * Math.sin(angle));
            
            this.cities.push({
                name: `City ${i+1}`,
                x: Math.max(0, Math.min(width, x)),
                y: Math.max(0, Math.min(height, y))
            });
        }
    }
    
    /**
     * Generate cities in a circular pattern
     */
    generateCircularCities(numCities, width, height) {
        const centerX = width / 2;
        const centerY = height / 2;
        const radius = Math.min(width, height) * 0.4;
        
        for (let i = 0; i < numCities; i++) {
            const angle = (i / numCities) * 2 * Math.PI;
            const noise = (Math.random() - 0.5) * 0.1 * radius;
            
            const x = Math.floor(centerX + (radius + noise) * Math.cos(angle));
            const y = Math.floor(centerY + (radius + noise) * Math.sin(angle));
            
            this.cities.push({
                name: `City ${i+1}`,
                x: x,
                y: y
            });
        }
    }
    
    /**
     * Run a simulation with different parameter combinations
     * @param {Array} cities - Array of city objects
     * @param {Object} parameterRanges - Ranges for different parameters to test
     * @param {Function} resultCallback - Callback for each result
     */
    async runParameterSweep(cities, parameterRanges, resultCallback) {
        this.simulationResults = [];
        const totalSimulations = this.calculateTotalSimulations(parameterRanges);
        let completedSimulations = 0;
        
        // Generate all parameter combinations
        const parameterCombinations = this.generateParameterCombinations(parameterRanges);
        
        for (const params of parameterCombinations) {
            this.currentSimulation = params;
            
            try {
                const result = await this.runSingleSimulation(cities, params);
                this.simulationResults.push({
                    parameters: params,
                    result: result
                });
                
                completedSimulations++;
                if (resultCallback) {
                    resultCallback({
                        progress: (completedSimulations / totalSimulations) * 100,
                        currentParams: params,
                        currentResult: result,
                        allResults: this.simulationResults
                    });
                }
            } catch (error) {
                console.error('Simulation error:', error);
            }
        }
        
        return this.simulationResults;
    }
    
    /**
     * Calculate how many simulations will be run based on parameter ranges
     */
    calculateTotalSimulations(parameterRanges) {
        let total = 1;
        for (const param in parameterRanges) {
            total *= parameterRanges[param].length;
        }
        return total;
    }
    
    /**
     * Generate all possible parameter combinations
     */
    generateParameterCombinations(parameterRanges) {
        const keys = Object.keys(parameterRanges);
        const combinations = [{}];
        
        for (const key of keys) {
            const values = parameterRanges[key];
            const newCombinations = [];
            
            for (const combination of combinations) {
                for (const value of values) {
                    newCombinations.push({
                        ...combination,
                        [key]: value
                    });
                }
            }
            
            combinations.length = 0;
            combinations.push(...newCombinations);
        }
        
        return combinations;
    }
    
    /**
     * Run a single simulation with the given parameters
     */
    async runSingleSimulation(cities, params) {
        try {
            const response = await fetch('/solve-tsp', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    cities: cities,
                    ...params
                })
            });
            
            if (!response.ok) {
                throw new Error(`Server responded with status ${response.status}`);
            }
            
            return await response.json();
        } catch (error) {
            console.error('Error running simulation:', error);
            throw error;
        }
    }
    
    /**
     * Get statistics and insights from simulation results
     */
    analyzeResults() {
        if (this.simulationResults.length === 0) {
            return null;
        }
        
        // Find best result
        const bestResult = this.simulationResults.reduce((best, current) => {
            return current.result.distance < best.result.distance ? current : best;
        }, this.simulationResults[0]);
        
        // Calculate average distance
        const averageDistance = this.simulationResults.reduce((sum, current) => {
            return sum + current.result.distance;
        }, 0) / this.simulationResults.length;
        
        // Group by different parameters to find trends
        const parameterKeys = Object.keys(this.simulationResults[0].parameters);
        const parameterAnalysis = {};
        
        for (const key of parameterKeys) {
            // Group results by this parameter
            const valueGroups = new Map();
            
            for (const simulation of this.simulationResults) {
                const paramValue = simulation.parameters[key];
                if (!valueGroups.has(paramValue)) {
                    valueGroups.set(paramValue, []);
                }
                valueGroups.get(paramValue).push(simulation.result);
            }
            
            // Calculate average distance for each parameter value
            const valueAnalysis = [];
            
            for (const [value, results] of valueGroups.entries()) {
                const avgDistance = results.reduce((sum, r) => sum + r.distance, 0) / results.length;
                valueAnalysis.push({
                    value: value,
                    averageDistance: avgDistance,
                    sampleSize: results.length
                });
            }
            
            parameterAnalysis[key] = valueAnalysis;
        }
        
        return {
            totalSimulations: this.simulationResults.length,
            bestResult: bestResult,
            averageDistance: averageDistance,
            parameterAnalysis: parameterAnalysis
        };
    }
    
    /**
     * Generate a benchmark problem with a known optimal solution
     */
    generateBenchmarkProblem(name) {
        switch (name) {
            case 'square':
                return this.generateSquareBenchmark();
            case 'circle':
                return this.generateCircleBenchmark();
            case 'spiral':
                return this.generateSpiralBenchmark();
            default:
                return this.generateSquareBenchmark();
        }
    }
    
    generateSquareBenchmark() {
        const cities = [];
        const n = 5; // 5x5 grid
        const spacing = 100;
        
        let index = 1;
        for (let i = 0; i < n; i++) {
            for (let j = 0; j < n; j++) {
                cities.push({
                    name: `City ${index}`,
                    x: j * spacing + 50,
                    y: i * spacing + 50
                });
                index++;
            }
        }
        
        // Known optimal solution (for a grid, it's going row by row or column by column)
        const optimalPath = cities.map(city => city.name);
        const optimalDistance = (n*n - 1) * spacing + spacing; // Approx distance going through all grid points and back
        
        this.bestKnownSolutions.set('square', {
            path: optimalPath,
            distance: optimalDistance
        });
        
        return cities;
    }
    
    generateCircleBenchmark() {
        const cities = [];
        const n = 20; // 20 points on a circle
        const radius = 300;
        const centerX = 400;
        const centerY = 300;
        
        for (let i = 0; i < n; i++) {
            const angle = (i / n) * 2 * Math.PI;
            cities.push({
                name: `City ${i+1}`,
                x: Math.floor(centerX + radius * Math.cos(angle)),
                y: Math.floor(centerY + radius * Math.sin(angle))
            });
        }
        
        // For a circle, optimal is to visit points in order around the circle
        const optimalPath = cities.map(city => city.name);
        
        // Calculate exact distance
        let distance = 0;
        for (let i = 0; i < n; i++) {
            const current = cities[i];
            const next = cities[(i + 1) % n];
            distance += Math.sqrt(Math.pow(next.x - current.x, 2) + Math.pow(next.y - current.y, 2));
        }
        
        this.bestKnownSolutions.set('circle', {
            path: optimalPath,
            distance: Math.round(distance)
        });
        
        return cities;
    }
    
    generateSpiralBenchmark() {
        const cities = [];
        const n = 20; // 20 points on a spiral
        const centerX = 400;
        const centerY = 300;
        const a = 10; // Spiral parameter
        const b = 0.2; // Spiral parameter
        
        for (let i = 0; i < n; i++) {
            const t = i * 0.5;
            const radius = a + b * t;
            const angle = t;
            
            cities.push({
                name: `City ${i+1}`,
                x: Math.floor(centerX + radius * Math.cos(angle) * 10),
                y: Math.floor(centerY + radius * Math.sin(angle) * 10)
            });
        }
        
        // For a spiral, optimal is to visit points in order along the spiral
        const optimalPath = cities.map(city => city.name);
        
        // Calculate exact distance
        let distance = 0;
        for (let i = 0; i < n; i++) {
            const current = cities[i];
            const next = cities[(i + 1) % n];
            distance += Math.sqrt(Math.pow(next.x - current.x, 2) + Math.pow(next.y - current.y, 2));
        }
        
        this.bestKnownSolutions.set('spiral', {
            path: optimalPath,
            distance: Math.round(distance)
        });
        
        return cities;
    }
    
    /**
     * Check solution quality compared to known optimal
     */
    evaluateSolution(problemName, solution) {
        if (!this.bestKnownSolutions.has(problemName)) {
            return { quality: "unknown", gap: null };
        }
        
        const optimal = this.bestKnownSolutions.get(problemName);
        const gap = (solution.distance - optimal.distance) / optimal.distance * 100;
        
        let quality;
        if (gap <= 0) {
            quality = "optimal";
        } else if (gap <= 5) {
            quality = "excellent";
        } else if (gap <= 10) {
            quality = "good";
        } else if (gap <= 20) {
            quality = "fair";
        } else {
            quality = "poor";
        }
        
        return {
            quality: quality,
            gap: gap,
            optimalDistance: optimal.distance,
            yourDistance: solution.distance
        };
    }
}

// Make the simulation class globally available
window.TSPSimulation = TSPSimulation; 