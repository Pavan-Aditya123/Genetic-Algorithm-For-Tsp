# Parallel Genetic Algorithm for Traveling Salesman Problem

This project implements a parallel genetic algorithm to solve the Traveling Salesman Problem (TSP) - one of the classic NP-Hard optimization problems in computer science. The implementation uses Haskell for the back-end computation with parallelization techniques and a modern interactive JavaScript front-end for visualization.

## Features

- **Parallel Genetic Algorithm**: Efficiently solves TSP using parallelization techniques in Haskell
- **Interactive Visualization**: Modern UI with real-time visualization of algorithm progress
- **Advanced Visualization Features**:
  - Path animation
  - Historical path tracking
  - Edge frequency heatmap
  - Interactive city placement and dragging
  - Zoom and pan functionality
- **Customizable Parameters**:
  - Population size
  - Number of generations
  - Mutation rate
  - Crossover rate
  - Elitism count
  - Tournament selection size
- **Analytics**:
  - Fitness evolution charts
  - Generation statistics
  - Real-time performance metrics
- **Preset City Collections**:
  - European capitals
  - US major cities
  - Custom grid pattern
- **Multiple Genetic Operators**:
  - Order crossover (OX)
  - Swap mutation
  - Inversion mutation

## Technical Details

### Genetic Algorithm Implementation

The genetic algorithm implementation includes:

1. **Population Initialization**: Random generation of valid TSP routes
2. **Fitness Function**: Based on the inverse of total route distance
3. **Selection**: Tournament selection for parent selection
4. **Crossover**: Order Crossover (OX) to preserve route validity
5. **Mutation**: Both swap mutation and inversion mutation
6. **Elitism**: Preserving the best individuals from each generation
7. **Parallelization**: Using Haskell's parallel strategies for efficient computation

### Visualization

The visualization system includes:

1. **Path Rendering**: Dynamic rendering of current and best paths
2. **City Interaction**: Ability to add, remove, and drag cities
3. **Historical View**: Option to see how the solution evolved
4. **Heatmap**: Visual representation of edge frequency in best solutions
5. **Metrics Dashboard**: Real-time display of algorithm performance
6. **Animation Controls**: Pause, resume, speed control for visualization

## Requirements

- GHC (Glasgow Haskell Compiler) 8.8.4 or higher
- Stack build system
- Node.js 14 or higher
- npm (Node Package Manager)

## Installation

1. Clone the repository:
   ```
   git clone https://github.com/yourusername/tsp-parallel-ga.git
   cd tsp-parallel-ga
   ```

2. Build the Haskell project:
   ```
   cd tsp-project
   stack build
   ```

3. Install Node.js dependencies:
   ```
   npm install
   ```

## Running the Application

1. Start the server:
   ```
   npm start
   ```

2. Open a web browser and navigate to:
   ```
   http://localhost:3000
   ```

## Command-Line Usage

The application can also be used from the command line:

```
stack run -- solve input.json output.json
```

Where `input.json` contains the TSP problem definition and parameters, and `output.json` will store the results.

## Input Format

The input JSON file should have the following structure:

```json
{
  "cities": [
    {"name": "City1", "x": 100, "y": 200},
    {"name": "City2", "x": 150, "y": 250},
    ...
  ],
  "populationSize": 100,
  "generations": 100,
  "mutationRate": 0.01,
  "crossoverRate": 0.9,
  "elitismCount": 2,
  "tournamentSize": 5
}
```

## How It Works

1. **Problem Definition**: Define the cities with coordinates.
2. **Algorithm Parameters**: Set the genetic algorithm parameters.
3. **Execution**: Run the algorithm with the "Start Algorithm" button.
4. **Visualization**: Watch as the algorithm evolves the solution.
5. **Results**: See the best path found, its distance, and performance metrics.

## Performance Optimization

The algorithm utilizes several optimization techniques:

1. **Parallel Evaluation**: Fitness calculations are performed in parallel.
2. **Efficient Data Structures**: Using arrays and bit vectors for route representation.
3. **Memory Management**: Careful handling of generations to avoid excessive memory usage.
4. **Caching**: Distance matrix pre-computation for faster fitness evaluation.

## License

This project is released under the BSD-3-Clause license.

## Acknowledgments

- The algorithm implementation draws inspiration from various genetic algorithm research papers.
- Visualization techniques are inspired by modern data visualization best practices.
- The parallel implementation leverages Haskell's excellent concurrency and parallelism capabilities. 