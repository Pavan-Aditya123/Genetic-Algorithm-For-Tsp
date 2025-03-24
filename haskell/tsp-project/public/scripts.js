let cities = []; // Stores the added cities
let cityId = 0; // Unique ID for each city

// Add city to the list
function addCity() {
    const cityName = document.getElementById('cityName').value;
    const cityX = parseInt(document.getElementById('cityX').value);
    const cityY = parseInt(document.getElementById('cityY').value);

    // Validate inputs
    if (!cityName || isNaN(cityX) || isNaN(cityY)) {
        alert('Please enter valid city details!');
        return;
    }

    // Create the city object and push to cities array
    const city = { id: cityId++, name: cityName, x: cityX, y: cityY };
    cities.push(city);
    displayCities();
    drawCities();
    clearInputs();
}

// Display added cities in the list
function displayCities() {
    const cityList = document.getElementById('citiesList');
    cityList.innerHTML = ''; // Clear the existing list

    cities.forEach(city => {
        const li = document.createElement('li');
        li.textContent = `${city.name} (X: ${city.x}, Y: ${city.y})`;
        cityList.appendChild(li);
    });
}

// Clear input fields
function clearInputs() {
    document.getElementById('cityName').value = '';
    document.getElementById('cityX').value = '';
    document.getElementById('cityY').value = '';
}

// Draw the cities on the canvas
function drawCities() {
    const canvas = document.getElementById('tspCanvas');
    const ctx = canvas.getContext('2d');
    ctx.clearRect(0, 0, canvas.width, canvas.height); // Clear previous drawings

    // Draw each city
    cities.forEach(city => {
        ctx.beginPath();
        ctx.arc(city.x * 10, city.y * 10, 5, 0, 2 * Math.PI); // Scale coordinates for canvas size
        ctx.fillStyle = 'blue';
        ctx.fill();
        ctx.stroke();
        ctx.fillText(city.name, city.x * 10 + 5, city.y * 10 + 5); // Display city name
    });
}

// Solve the TSP by calling the backend
async function solveTSP() {
    if (cities.length < 2) {
        alert('Please add at least two cities!');
        return;
    }

    const resultDiv = document.getElementById('result');
    const pathParagraph = document.getElementById('optimalPath');
    const distanceParagraph = document.getElementById('totalDistance');

    const citiesData = cities.map(city => ({
        name: city.name,
        x: city.x,
        y: city.y
    }));

    console.log("Sending cities data to the server:", citiesData); // Debug log

    try {
        const response = await fetch('http://localhost:3000/solve-tsp', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ cities: citiesData })
        });

        console.log("Received response from server:", response); // Debug log

        if (!response.ok) {
            alert('Error solving TSP');
            console.log('Response not ok:', response);
            return;
        }

        const result = await response.json();
        console.log("TSP result:", result);  // Log the response for debugging

        // Update the optimal path and total distance
        pathParagraph.textContent = `Optimal Path: ${result.path.join(' -> ')}`;
        distanceParagraph.textContent = `Total Distance: ${result.distance} units`;

        // Draw the optimal path on the canvas
        drawOptimalPath(result.path);

        resultDiv.classList.remove('hidden');
    } catch (error) {
        alert('There was an error communicating with the server!');
        console.error("Error in API call:", error);
    }
}

// Draw the optimal path on the canvas
function drawOptimalPath(path) {
    const canvas = document.getElementById('tspCanvas');
    const ctx = canvas.getContext('2d');

    if (path.length < 2) return;

    ctx.beginPath();
    ctx.moveTo(cities.find(city => city.name === path[0]).x * 10, cities.find(city => city.name === path[0]).y * 10);

    // Draw the optimal path
    path.forEach(cityName => {
        const city = cities.find(city => city.name === cityName);
        ctx.lineTo(city.x * 10, city.y * 10);
    });
    ctx.lineTo(cities.find(city => city.name === path[0]).x * 10, cities.find(city => city.name === path[0]).y * 10); // Close the path
    ctx.strokeStyle = 'red';
    ctx.lineWidth = 2;
    ctx.stroke();
}
