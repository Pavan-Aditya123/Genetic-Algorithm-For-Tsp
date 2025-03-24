# Parallel Genetic Algorithm for TSP in Haskell

This project implements a Parallel Genetic Algorithm for solving the Traveling Salesman Problem (TSP) using Haskell for the backend computation and a JavaScript/HTML frontend for visualization.

## Overview

The Traveling Salesman Problem is a classic NP-hard optimization problem: given a set of cities and the distances between them, find the shortest possible route that visits each city exactly once and returns to the starting city.

This implementation uses:
- Haskell for the parallel genetic algorithm implementation
- Node.js/Express for the web server
- HTML/CSS/JavaScript for the frontend visualization

## Features

- Parallel implementation of a genetic algorithm using Haskell's parallel processing capabilities
- Interactive web-based visualization of the solution progress
- Real-time progress tracking
- Multiple mutation and crossover operators for better optimization
- Visualization of the evolution history and fitness improvement
- Predefined city sets and ability to create custom problems

## Requirements

- [Node.js](https://nodejs.org/) (v14+ recommended)
- [Haskell](https://www.haskell.org/ghcup/) with GHC and Cabal
- Modern web browser

## Installation

1. Clone the repository:
   ```
   git clone <repository-url>
   cd tsp-project
   ```

2. Install Node.js dependencies:
   ```
   npm install
   ```

3. Set up the project (build Haskell code and create presets):
   ```
   npm run setup
   ```

4. Start the server:
   ```
   npm start
   ```

5. Open your browser and navigate to:
   ```
   http://localhost:3000
   ```

## Project Structure

- `app/` - Haskell source code for the genetic algorithm
  - `Main.hs` - Main entry point and API endpoints
  - `GeneticAlgorithm.hs` - Implementation of the genetic algorithm

- `public/` - Frontend files
  - `index.html` - Main web interface
  - `styles.css` - Styling for the web interface
  - `scripts.js` - Core JavaScript functionality
  - `visualization.js` - Visualization of TSP solutions
  - `simulation.js` - JavaScript fallback implementation
  - `presets/` - Predefined city sets

- `server.js` - Node.js Express server
- `server.hs` - Alternative Haskell-based server (optional)

## How It Works

### Genetic Algorithm

The project implements a genetic algorithm with the following components:

1. **Representation**: A chromosome is represented as a permutation of cities.
2. **Initial Population**: Random permutations of cities.
3. **Fitness Function**: The inverse of the total route distance.
4. **Selection**: Tournament selection.
5. **Crossover**: Order Crossover (OX).
6. **Mutation**: Swap Mutation and Inversion Mutation.
7. **Replacement**: Elitism and generational replacement.

### Parallelism

The genetic algorithm leverages Haskell's parallelism capabilities:

- Population evaluation is done in parallel
- Multiple mutation operators run concurrently
- Fitness calculations are distributed across CPU cores

### API Endpoints

- `POST /solve-tsp` - Submit a TSP problem for solving
- `GET /progress/:jobId` - Get progress of a running job
- `GET /result` - Get the final result of the latest job
- `GET /city-presets/:name` - Get a predefined set of cities

## Usage

1. Open the web interface at http://localhost:3000
2. Select a predefined city set or create your own by clicking on the map
3. Adjust the genetic algorithm parameters if needed
4. Click "Solve TSP" to start the optimization
5. Watch the real-time visualization of the best route and evolution progress
6. View the final result and statistics

## Customization

You can customize the genetic algorithm parameters:

- Population size
- Number of generations
- Mutation rate
- Crossover rate
- Elitism count
- Tournament size

## License

This project is licensed under the ISC License - see the LICENSE file for details.

## Acknowledgments

- This project was inspired by various implementations of genetic algorithms for solving TSP.
- Thanks to the Haskell community for providing excellent parallelism libraries. 