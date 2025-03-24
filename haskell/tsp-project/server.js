const express = require('express');
const path = require('path');
const { spawn } = require('child_process');
const bodyParser = require('body-parser');
const fs = require('fs');
const cors = require('cors');

const app = express();
const port = process.env.PORT || 3000;

// Middleware
app.use(cors());
app.use(bodyParser.json({ limit: '10mb' }));
app.use(express.static(path.join(__dirname, 'public')));

// Store running jobs
const runningJobs = new Map();

// Endpoint to serve the main page
app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, 'public', 'index.html'));
});

// Endpoint to solve TSP with GA using Haskell backend
app.post('/solve-tsp', (req, res) => {
  const tspData = req.body;
  const jobId = Date.now().toString();
  
  console.log('Received TSP request:', JSON.stringify(tspData.cities.length) + ' cities');
  
  // Prepare the input data in the correct format
  let inputData = { ...tspData };
  
  // Check if parameters are provided in a nested object and convert to the expected format
  if (tspData.parameters) {
    // Remove the parameters object and spread its properties at the top level
    const { parameters, ...rest } = tspData;
    inputData = {
      ...rest,
      // Add each parameter at the top level if present
      ...(parameters.populationSize !== undefined && { populationSize: parameters.populationSize }),
      ...(parameters.generations !== undefined && { generations: parameters.generations }),
      ...(parameters.mutationRate !== undefined && { mutationRate: parameters.mutationRate }),
      ...(parameters.crossoverRate !== undefined && { crossoverRate: parameters.crossoverRate }),
      ...(parameters.elitismCount !== undefined && { elitismCount: parameters.elitismCount }),
      ...(parameters.tournamentSize !== undefined && { tournamentSize: parameters.tournamentSize })
    };
  }
  
  // Save the request data to a temporary file
  try {
    fs.writeFileSync('public/input.json', JSON.stringify(inputData));
    console.log('Input data saved successfully');
  } catch (error) {
    console.error('Error saving input data:', error);
    return res.status(500).json({ error: 'Failed to save input data' });
  }
  
  console.log('Running Haskell TSP solver');
  
  // Set initial progress
  runningJobs.set(jobId, { progress: 0, startTime: Date.now() });
  
  // Start the Haskell process
  const haskellProcess = spawn('cabal', ['run', 'tsp-project', '--', 'solve', 'public/input.json', 'public/output.json']);
  
  // Log Haskell output
  haskellProcess.stdout.on('data', (data) => {
    const output = data.toString();
    console.log(`Haskell Output: ${output}`);
    
    // Check for progress information - try to match both formats
    const progressMatch = output.match(/Progress: (\d+)%/) || output.match(/(\d+)%/);
    if (progressMatch && progressMatch[1]) {
      const progress = parseInt(progressMatch[1]);
      console.log(`Progress update received: ${progress}%`);
      runningJobs.set(jobId, { 
        progress, 
        startTime: runningJobs.get(jobId).startTime 
      });
    }
    
    // If process is complete but no progress was reported, set to 100%
    if (output.includes('Completed') || output.includes('Done') || output.includes('Finished')) {
      console.log('Completion message detected, setting progress to 100%');
      runningJobs.set(jobId, { 
        progress: 100, 
        startTime: runningJobs.get(jobId).startTime 
      });
    }
  });
  
  haskellProcess.stderr.on('data', (data) => {
    const errorOutput = data.toString();
    console.error(`Haskell Error: ${errorOutput}`);
    
    // Only update job with error if it's a real error
    // Some Haskell implementations output compilation info to stderr
    if (errorOutput.includes('error:') || errorOutput.includes('Exception:')) {
      runningJobs.set(jobId, { 
        progress: -1, // Error state
        startTime: runningJobs.get(jobId).startTime,
        error: `Haskell Error: ${errorOutput}`
      });
    }
  });
  
  haskellProcess.on('close', (code) => {
    console.log(`Haskell process exited with code ${code}`);
    
    if (code === 0) {
      // Process completed successfully
      try {
        // Verify the output file exists and is readable
        if (fs.existsSync('public/output.json')) {
          // Read the file to verify it's valid JSON
          const outputContent = fs.readFileSync('public/output.json', 'utf8');
          try {
            JSON.parse(outputContent); // This will throw if not valid JSON
            // Update progress to 100%
            runningJobs.set(jobId, { 
              progress: 100, 
              startTime: runningJobs.get(jobId).startTime 
            });
            console.log('Output file verified and job marked as complete');
          } catch (jsonError) {
            console.error('Output file contains invalid JSON:', jsonError);
            runningJobs.set(jobId, { 
              progress: -1, 
              startTime: runningJobs.get(jobId).startTime,
              error: 'Output file contains invalid JSON'
            });
          }
        } else {
          console.error('Output file does not exist after Haskell process completed');
          runningJobs.set(jobId, { 
            progress: -1, 
            startTime: runningJobs.get(jobId).startTime,
            error: 'Output file not created'
          });
        }
        
        // Schedule cleanup of the job info
        setTimeout(() => {
          runningJobs.delete(jobId);
        }, 60000); // Clean up after 1 minute
        
      } catch (error) {
        console.error('Error reading output file:', error);
        runningJobs.set(jobId, { 
          progress: -1, // Error state
          startTime: runningJobs.get(jobId).startTime,
          error: error.message
        });
      }
    } else {
      // Process failed
      runningJobs.set(jobId, { 
        progress: -1, // Error state
        startTime: runningJobs.get(jobId).startTime,
        error: `Haskell process exited with code ${code}`
      });
    }
  });
  
  // Return the job ID
  res.json({ jobId });
});

// Alternative endpoint to use JavaScript simulation if Haskell backend fails
app.post('/solve-tsp-js', (req, res) => {
  const tspData = req.body;
  console.log('Using JavaScript TSP solver as fallback');
  
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
    
    // Calculate total distance for this route (including return to start)
    let totalDistance = 0;
    for (let i = 0; i < bestPath.length; i++) {
      const city1 = cities[bestPath[i]];
      const city2 = cities[bestPath[(i + 1) % bestPath.length]];
      const dx = city1.x - city2.x;
      const dy = city1.y - city2.y;
      totalDistance += Math.sqrt(dx * dx + dy * dy);
    }
    
    // Add the distance from the last city back to the first city
    const firstCity = cities[bestPath[0]];
    const lastCity = cities[bestPath[bestPath.length - 1]];
    const dx = lastCity.x - firstCity.x;
    const dy = lastCity.y - firstCity.y;
    const returnDistance = Math.sqrt(dx * dx + dy * dy);
    totalDistance += returnDistance;
    
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
        bestFitness: 1.0 / currentBest,
        averageFitness: 1.0 / (currentBest * 1.2),
        bestPath: [...bestPath] // Clone the array
      });
    }
    
    // Save the result
    const result = {
      bestPath: bestPath,
      bestDistance: Math.round(totalDistance),
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
  
  if (runningJobs.has(jobId)) {
    const jobInfo = runningJobs.get(jobId);
    const elapsedTime = Date.now() - jobInfo.startTime;
    
    // Add estimated time remaining if progress is between 0 and 100
    let response = { progress: jobInfo.progress };
    
    if (jobInfo.progress > 0 && jobInfo.progress < 100) {
      const estimatedTotalTime = (elapsedTime / jobInfo.progress) * 100;
      const estimatedTimeRemaining = Math.max(0, estimatedTotalTime - elapsedTime);
      response.estimatedTimeRemaining = Math.round(estimatedTimeRemaining / 1000);
    }
    
    if (jobInfo.error) {
      response.error = jobInfo.error;
    }
    
    res.json(response);
  } else {
    // Job not found, either completed or invalid
    res.json({ progress: 100 });
  }
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
  
  try {
    const presetFile = path.join(__dirname, 'public', 'presets', `${presetName}.json`);
    const presetData = fs.readFileSync(presetFile, 'utf8');
    res.json(JSON.parse(presetData));
  } catch (error) {
    res.status(404).json({ error: 'Preset not found' });
  }
});

// Start the server
app.listen(port, () => {
  console.log(`⚡ Server running at http://localhost:${port}`);
  console.log(`📊 TSP Genetic Algorithm Visualization`);
  
  // Check if Haskell is available
  const haskellCheck = spawn('cabal', ['--version']);
  
  haskellCheck.on('close', (code) => {
    if (code === 0) {
      console.log('✅ Haskell backend is available');
    } else {
      console.warn('⚠️ Haskell backend not detected, will use JavaScript fallback');
    }
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
});
