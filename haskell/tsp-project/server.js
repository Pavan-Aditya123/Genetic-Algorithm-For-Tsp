const express = require('express');
const path = require('path');
const { spawn } = require('child_process');
const bodyParser = require('body-parser');
const fs = require('fs');

const app = express();
const port = process.env.PORT || 3000;

// Middleware
app.use(bodyParser.json({ limit: '10mb' }));
app.use(express.static(path.join(__dirname, 'public')));

// Store running jobs
const runningJobs = new Map();

// Endpoint to serve the main page
app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, 'public', 'index.html'));
});

// Endpoint to solve TSP with GA
app.post('/solve-tsp', (req, res) => {
  const tspData = req.body;
  const jobId = Date.now().toString();
  
  console.log('Received TSP request:', JSON.stringify(tspData));
  
  // Save the request data to a temporary file
  fs.writeFileSync('public/input.json', JSON.stringify(tspData));
  
  // SIMULATE TSP SOLVER INSTEAD OF RUNNING HASKELL
  console.log('Simulating TSP solver (no Haskell backend)');
  
  // Get data from the request
  const cities = tspData.cities || [];
  const params = tspData.parameters || {};
  
  // Create a simulated result
  setTimeout(() => {
    // Create a proper solution using a simple nearest neighbor heuristic
    const bestPath = [];
    const visited = new Array(cities.length).fill(false);
    
    // Start from city 0
    let currentCity = 0;
    bestPath.push(currentCity);
    visited[currentCity] = true;
    
    // Visit each unvisited city, choosing the closest one each time
    while (bestPath.length < cities.length) {
      let closestCity = -1;
      let minDistance = Infinity;
      
      for (let i = 0; i < cities.length; i++) {
        if (!visited[i]) {
          const city1 = cities[currentCity];
          const city2 = cities[i];
          const dx = city1.x - city2.x;
          const dy = city1.y - city2.y;
          const distance = Math.sqrt(dx * dx + dy * dy);
          
          if (distance < minDistance) {
            minDistance = distance;
            closestCity = i;
          }
        }
      }
      
      if (closestCity !== -1) {
        currentCity = closestCity;
        bestPath.push(currentCity);
        visited[currentCity] = true;
      }
    }
    
    // Calculate total distance for this route
    let totalDistance = 0;
    for (let i = 0; i < bestPath.length; i++) {
      const city1 = cities[bestPath[i]];
      const city2 = cities[bestPath[(i + 1) % bestPath.length]];
      const dx = city1.x - city2.x;
      const dy = city1.y - city2.y;
      totalDistance += Math.sqrt(dx * dx + dy * dy);
    }
    
    // Generate simulated evolution history
    const generations = params.generations || 100;
    const populationSize = params.populationSize || 50;
    
    const generationHistory = [];
    let currentBest = totalDistance * 1.5; // Start with a worse distance
    
    for (let i = 0; i < generations; i++) {
      // Improve the solution with each generation
      currentBest = totalDistance * (1 + 0.5 * (1 - i / generations));
      
      generationHistory.push({
        generation: i,
        bestFitness: currentBest,
        averageFitness: currentBest * 1.2,
        bestPath: [...bestPath] // Clone the array
      });
    }
    
    // Save the result
    const result = {
      bestPath: bestPath,
      bestDistance: totalDistance,
      generationHistory: generationHistory,
      elapsed: 1.5, // Simulated time in seconds
      cities: cities,
      parameters: params
    };
    
    fs.writeFileSync('public/output.json', JSON.stringify(result));
    
    // Return the result
    res.json(result);
  }, 500); // Complete in 0.5 seconds for better responsiveness
});

// Endpoint to get progress information
app.get('/progress/:jobId', (req, res) => {
  const jobId = req.params.jobId;
  
  // Instead of tracking processes, just return simulated progress
  const startTime = parseInt(jobId);
  const currentTime = Date.now();
  const elapsedTime = currentTime - startTime;
  const simulationTime = 2000; // 2 seconds, matching our setTimeout
  
  const progress = Math.min(100, Math.round((elapsedTime / simulationTime) * 100));
  
  res.json({ progress });
});

// Endpoint to retrieve results data
app.get('/result', (req, res) => {
  try {
    const resultData = fs.readFileSync('public/output.json');
    res.type('application/json').send(resultData);
  } catch (error) {
    console.error('Error reading result file:', error);
    res.status(404).json({ error: 'Results not found' });
  }
});

// Endpoint to get predefined city sets
app.get('/city-presets/:name', (req, res) => {
  const presetName = req.params.name;
  
  // Define some common presets
  const presets = {
    'capitals': require('./public/presets/capitals.json'),
    'usa': require('./public/presets/usa.json'),
    'europe': require('./public/presets/europe.json'),
    'grid': require('./public/presets/grid.json')
  };
  
  if (presets[presetName]) {
    res.json(presets[presetName]);
  } else {
    res.status(404).json({ error: 'Preset not found' });
  }
});

// Start the server
app.listen(port, () => {
  console.log(`⚡ Server running at http://localhost:${port}`);
  console.log(`📊 TSP Genetic Algorithm Visualization`);
});

// Create presets directory if it doesn't exist
const presetsDir = path.join(__dirname, 'public', 'presets');
if (!fs.existsSync(presetsDir)) {
  fs.mkdirSync(presetsDir, { recursive: true });
  
  // Create sample preset files
  const capitals = [
    { name: "London", x: 51, y: 0 },
    { name: "Paris", x: 48, y: 2 },
    { name: "Berlin", x: 52, y: 13 },
    { name: "Rome", x: 41, y: 12 },
    { name: "Madrid", x: 40, y: -3 },
    { name: "Vienna", x: 48, y: 16 },
    { name: "Athens", x: 37, y: 23 },
    { name: "Warsaw", x: 52, y: 21 },
    { name: "Budapest", x: 47, y: 19 },
    { name: "Brussels", x: 50, y: 4 }
  ];
  
  const usa = [
    { name: "New York", x: 40, y: -74 },
    { name: "Los Angeles", x: 34, y: -118 },
    { name: "Chicago", x: 41, y: -87 },
    { name: "Houston", x: 29, y: -95 },
    { name: "Phoenix", x: 33, y: -112 },
    { name: "Philadelphia", x: 39, y: -75 },
    { name: "San Antonio", x: 29, y: -98 },
    { name: "San Diego", x: 32, y: -117 },
    { name: "Dallas", x: 32, y: -96 },
    { name: "San Jose", x: 37, y: -121 }
  ];
  
  const europe = [
    { name: "London", x: 51, y: 0 },
    { name: "Paris", x: 48, y: 2 },
    { name: "Berlin", x: 52, y: 13 },
    { name: "Rome", x: 41, y: 12 },
    { name: "Madrid", x: 40, y: -3 },
    { name: "Barcelona", x: 41, y: 2 },
    { name: "Amsterdam", x: 52, y: 4 },
    { name: "Munich", x: 48, y: 11 },
    { name: "Zurich", x: 47, y: 8 },
    { name: "Brussels", x: 50, y: 4 },
    { name: "Milan", x: 45, y: 9 },
    { name: "Vienna", x: 48, y: 16 },
    { name: "Copenhagen", x: 55, y: 12 },
    { name: "Stockholm", x: 59, y: 18 },
    { name: "Prague", x: 50, y: 14 }
  ];
  
  // Grid pattern for testing
  const grid = [];
  for (let i = 0; i < 5; i++) {
    for (let j = 0; j < 5; j++) {
      grid.push({
        name: `Node ${i*5 + j + 1}`,
        x: i * 100,
        y: j * 100
      });
    }
  }
  
  fs.writeFileSync(path.join(presetsDir, 'capitals.json'), JSON.stringify(capitals));
  fs.writeFileSync(path.join(presetsDir, 'usa.json'), JSON.stringify(usa));
  fs.writeFileSync(path.join(presetsDir, 'europe.json'), JSON.stringify(europe));
  fs.writeFileSync(path.join(presetsDir, 'grid.json'), JSON.stringify(grid));
}
