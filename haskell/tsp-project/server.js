const express = require('express');
const bodyParser = require('body-parser');
const path = require('path'); // To help resolve paths to static assets
const app = express();
const port = 3000;

// Middleware to parse JSON requests
app.use(bodyParser.json());

// Serve static files from the 'public' directory
app.use(express.static(path.join(__dirname, 'public')));

// Sample TSP algorithm (You can replace this with a better algorithm)
function solveTSPAlgorithm(cities) {
    // Placeholder example logic: Just returns the cities in the order they are given
    const path = cities.map(city => city.name);
    const distance = cities.length * 10; // Placeholder distance calculation

    return { path, distance };
}

// Endpoint to solve TSP
app.post('/solve-tsp', (req, res) => {
    const cities = req.body.cities;
    const result = solveTSPAlgorithm(cities);  // Call your TSP algorithm here
    res.json(result);  // Return path and distance
});

// Serve the main page (index.html) when accessing the root URL
app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, 'index.html')); // Replace with the correct path to your HTML file
});

// Start the server
app.listen(port, () => {
    console.log(`Server is running on http://localhost:${port}`);
});
