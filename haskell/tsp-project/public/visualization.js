// TSP Genetic Algorithm Visualization
// Advanced visualization for the TSP genetic algorithm with animations and real-time metrics

class TSPVisualization {
    constructor(canvasId) {
        this.canvas = document.getElementById(canvasId);
        this.ctx = this.canvas.getContext('2d');
        this.cities = [];
        this.currentPath = [];
        this.bestPath = [];
        this.historicalPaths = [];
        this.historicalFitness = [];
        this.animationFrame = null;
        this.isPaused = false;
        this.animationSpeed = 50;
        this.currentGeneration = 0;
        this.maxGenerations = 0;
        this.bestDistance = Infinity;
        this.currentDistance = 0;
        this.currentFitness = 0;
        this.showHistoricalPaths = false;
        this.heatmap = false;
        this.zoomLevel = 1;
        this.panOffset = { x: 0, y: 0 };
        this.initialPopulation = [];
        this.populationSize = 0;
        this.cityRadius = 6;
        this.cityHoverRadius = 12;
        this.hoveredCity = null;
        this.draggingCity = null;
        this.selectedCity = null;
        this.cityColors = [];
        this.generationStats = [];
        this.progressCallback = null;
        
        // Initialize event listeners
        this.initEventListeners();
        
        // Generate random colors for cities
        this.generateCityColors();
    }
    
    // Initialize event listeners for interaction
    initEventListeners() {
        this.canvas.addEventListener('mousemove', this.handleMouseMove.bind(this));
        this.canvas.addEventListener('mousedown', this.handleMouseDown.bind(this));
        this.canvas.addEventListener('mouseup', this.handleMouseUp.bind(this));
        this.canvas.addEventListener('wheel', this.handleWheel.bind(this));
        window.addEventListener('resize', this.resizeCanvas.bind(this));
    }
    
    // Resize canvas to fit container
    resizeCanvas() {
        const container = this.canvas.parentElement;
        this.canvas.width = container.clientWidth;
        this.canvas.height = container.clientHeight;
        this.draw();
    }
    
    // Generate random colors for cities
    generateCityColors() {
        const colors = [
            '#3498db', '#e74c3c', '#2ecc71', '#f39c12', '#9b59b6',
            '#1abc9c', '#d35400', '#34495e', '#16a085', '#c0392b'
        ];
        
        this.cityColors = [];
        for (let i = 0; i < 100; i++) {
            this.cityColors.push(colors[i % colors.length]);
        }
    }
    
    // Set cities data
    setCities(cities) {
        this.cities = cities;
        this.currentPath = [];
        this.bestPath = [];
        this.draw();
    }
    
    // Set paths and metrics
    setPaths(currentPath, bestPath, distance, fitness, generation) {
        this.currentPath = currentPath;
        
        // Only update best path if it's better than current best
        if (distance < this.bestDistance) {
            this.bestPath = bestPath;
            this.bestDistance = distance;
        }
        
        this.currentDistance = distance;
        this.currentFitness = fitness;
        this.currentGeneration = generation;
        
        // Store historical data for visualization
        if (this.historicalPaths.length < 100) { // Limit storage to avoid memory issues
            this.historicalPaths.push([...bestPath]);
            this.historicalFitness.push(fitness);
        } else {
            this.historicalPaths.shift();
            this.historicalPaths.push([...bestPath]);
            this.historicalFitness.shift();
            this.historicalFitness.push(fitness);
        }
        
        // Store generation stats
        this.generationStats.push({
            generation: generation,
            distance: distance,
            fitness: fitness
        });
        
        this.draw();
        
        // Call progress callback if set
        if (this.progressCallback) {
            this.progressCallback(this.currentGeneration / this.maxGenerations * 100);
        }
    }
    
    // Set progress callback
    setProgressCallback(callback) {
        this.progressCallback = callback;
    }
    
    // Set algorithm parameters
    setParameters(popSize, maxGen) {
        this.populationSize = popSize;
        this.maxGenerations = maxGen;
    }
    
    // Set initial population
    setInitialPopulation(population) {
        this.initialPopulation = population;
    }
    
    // Draw current state
    draw() {
        if (!this.ctx) return;
        
        this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
        
        // Apply zoom and pan transformations
        this.ctx.save();
        this.ctx.translate(this.panOffset.x, this.panOffset.y);
        this.ctx.scale(this.zoomLevel, this.zoomLevel);
        
        // Draw heatmap if enabled
        if (this.heatmap && this.historicalPaths.length > 0) {
            this.drawHeatmap();
        }
        
        // Draw historical paths if enabled
        if (this.showHistoricalPaths) {
            this.drawHistoricalPaths();
        }
        
        // Draw current path
        if (this.currentPath.length > 1) {
            this.drawPath(this.currentPath, '#3498db', 2);
        }
        
        // Draw best path
        if (this.bestPath.length > 1) {
            this.drawPath(this.bestPath, '#2ecc71', 3);
        }
        
        // Draw cities
        this.drawCities();
        
        this.ctx.restore();
        
        // Draw UI elements
        this.drawUI();
    }
    
    // Draw historical paths to show evolution
    drawHistoricalPaths() {
        for (let i = 0; i < this.historicalPaths.length; i++) {
            const path = this.historicalPaths[i];
            const opacity = i / this.historicalPaths.length;
            this.ctx.globalAlpha = 0.1 + (opacity * 0.2);
            this.drawPath(path, '#3498db', 1);
        }
        this.ctx.globalAlpha = 1;
    }
    
    // Draw heatmap to show frequently used edges
    drawHeatmap() {
        // Create a map to count edge frequency
        const edgeCount = new Map();
        
        // Count edges in all historical paths
        for (const path of this.historicalPaths) {
            for (let i = 0; i < path.length; i++) {
                const from = path[i];
                const to = path[(i + 1) % path.length];
                
                // Create a unique key for the edge (smaller index first)
                const edge = from < to ? `${from}-${to}` : `${to}-${from}`;
                
                // Increment edge count
                if (edgeCount.has(edge)) {
                    edgeCount.set(edge, edgeCount.get(edge) + 1);
                } else {
                    edgeCount.set(edge, 1);
                }
            }
        }
        
        // Find max count for normalization
        const maxCount = Math.max(...edgeCount.values());
        
        // Draw edges with color intensity based on frequency
        this.ctx.globalAlpha = 0.3;
        for (const [edge, count] of edgeCount.entries()) {
            const [fromIdx, toIdx] = edge.split('-').map(Number);
            const from = this.cities[fromIdx];
            const to = this.cities[toIdx];
            
            if (from && to) {
                const fromX = this.scaleX(from.x);
                const fromY = this.scaleY(from.y);
                const toX = this.scaleX(to.x);
                const toY = this.scaleY(to.y);
                
                // Normalize count and map to color intensity
                const intensity = count / maxCount;
                const r = Math.floor(255 * intensity);
                const g = Math.floor(100 * (1 - intensity));
                const b = Math.floor(100 * (1 - intensity));
                
                this.ctx.beginPath();
                this.ctx.moveTo(fromX, fromY);
                this.ctx.lineTo(toX, toY);
                this.ctx.strokeStyle = `rgb(${r}, ${g}, ${b})`;
                this.ctx.lineWidth = 1 + (3 * intensity);
                this.ctx.stroke();
            }
        }
        this.ctx.globalAlpha = 1;
    }
    
    // Draw a path with given color and width
    drawPath(path, color, width) {
        if (path.length < 2 || this.cities.length < 2) return;
        
        this.ctx.beginPath();
        
        const startCity = this.cities[path[0]];
        if (!startCity) return;
        
        this.ctx.moveTo(this.scaleX(startCity.x), this.scaleY(startCity.y));
        
        for (let i = 1; i < path.length; i++) {
            const cityIdx = path[i];
            const city = this.cities[cityIdx];
            if (city) {
                this.ctx.lineTo(this.scaleX(city.x), this.scaleY(city.y));
            }
        }
        
        // Complete the loop
        if (path.length > 2) {
            this.ctx.lineTo(this.scaleX(startCity.x), this.scaleY(startCity.y));
        }
        
        this.ctx.strokeStyle = color;
        this.ctx.lineWidth = width;
        this.ctx.stroke();
    }
    
    // Draw cities with labels
    drawCities() {
        this.cities.forEach((city, index) => {
            const x = this.scaleX(city.x);
            const y = this.scaleY(city.y);
            
            // Draw city circle
            this.ctx.beginPath();
            
            const isHovered = this.hoveredCity === index;
            const isSelected = this.selectedCity === index;
            const radius = isHovered || isSelected ? this.cityHoverRadius : this.cityRadius;
            
            this.ctx.arc(x, y, radius, 0, 2 * Math.PI);
            
            // Set fill style based on state
            if (isSelected) {
                this.ctx.fillStyle = '#f1c40f'; // Yellow for selected
            } else if (isHovered) {
                this.ctx.fillStyle = '#e74c3c'; // Red for hover
            } else {
                this.ctx.fillStyle = this.cityColors[index % this.cityColors.length];
            }
            
            this.ctx.fill();
            
            // Draw border
            this.ctx.strokeStyle = '#2c3e50';
            this.ctx.lineWidth = 1;
            this.ctx.stroke();
            
            // Draw city name
            this.ctx.fillStyle = '#2c3e50';
            this.ctx.font = '12px Arial';
            this.ctx.textAlign = 'center';
            this.ctx.fillText(city.name, x, y - radius - 5);
            
            // Draw position in path if city is part of the current path
            const pathIdx = this.currentPath.indexOf(index);
            if (pathIdx !== -1) {
                this.ctx.fillStyle = 'white';
                this.ctx.font = 'bold 10px Arial';
                this.ctx.fillText(pathIdx + 1, x, y + 3);
            }
        });
    }
    
    // Draw UI elements (metrics, controls)
    drawUI() {
        // Draw metrics panel
        const padding = 10;
        const panelWidth = 200;
        const panelHeight = 120;
        const x = this.canvas.width - panelWidth - padding;
        const y = padding;
        
        // Draw panel background
        this.ctx.fillStyle = 'rgba(44, 62, 80, 0.8)';
        this.ctx.fillRect(x, y, panelWidth, panelHeight);
        
        // Draw metrics
        this.ctx.fillStyle = 'white';
        this.ctx.font = '12px Arial';
        this.ctx.textAlign = 'left';
        
        const metrics = [
            `Generation: ${this.currentGeneration}/${this.maxGenerations}`,
            `Current Distance: ${this.currentDistance.toFixed(2)}`,
            `Best Distance: ${this.bestDistance.toFixed(2)}`,
            `Fitness: ${this.currentFitness.toFixed(4)}`,
            `Cities: ${this.cities.length}`,
            `Population: ${this.populationSize}`
        ];
        
        metrics.forEach((metric, i) => {
            this.ctx.fillText(metric, x + 10, y + 20 + (i * 18));
        });
    }
    
    // Handle mouse move for hover effects
    handleMouseMove(event) {
        const rect = this.canvas.getBoundingClientRect();
        const mouseX = (event.clientX - rect.left) / this.zoomLevel - this.panOffset.x / this.zoomLevel;
        const mouseY = (event.clientY - rect.top) / this.zoomLevel - this.panOffset.y / this.zoomLevel;
        
        // Check if hovering over a city
        const hoveredIndex = this.cities.findIndex(city => {
            const cityX = this.scaleX(city.x);
            const cityY = this.scaleY(city.y);
            const distance = Math.sqrt((cityX - mouseX) ** 2 + (cityY - mouseY) ** 2);
            return distance <= this.cityHoverRadius;
        });
        
        if (hoveredIndex !== this.hoveredCity) {
            this.hoveredCity = hoveredIndex !== -1 ? hoveredIndex : null;
            this.draw();
        }
        
        // Handle city dragging
        if (this.draggingCity !== null) {
            const city = this.cities[this.draggingCity];
            if (city) {
                // Convert mouse coordinates back to data coordinates
                const newX = this.inverseScaleX(mouseX);
                const newY = this.inverseScaleY(mouseY);
                
                // Update city coordinates
                city.x = newX;
                city.y = newY;
                
                this.draw();
            }
        }
    }
    
    // Handle mouse down for city selection and dragging
    handleMouseDown(event) {
        if (this.hoveredCity !== null) {
            this.selectedCity = this.hoveredCity;
            this.draggingCity = this.hoveredCity;
            this.draw();
        } else {
            this.selectedCity = null;
            this.draw();
        }
    }
    
    // Handle mouse up to end dragging
    handleMouseUp() {
        this.draggingCity = null;
    }
    
    // Handle mouse wheel for zooming
    handleWheel(event) {
        event.preventDefault();
        
        const delta = event.deltaY < 0 ? 1.1 : 0.9;
        this.zoomLevel *= delta;
        
        // Limit zoom level
        this.zoomLevel = Math.max(0.1, Math.min(5, this.zoomLevel));
        
        this.draw();
    }
    
    // Scale X coordinate based on data range
    scaleX(x) {
        const padding = 50;
        const minX = Math.min(...this.cities.map(c => c.x));
        const maxX = Math.max(...this.cities.map(c => c.x));
        
        if (minX === maxX) return this.canvas.width / 2;
        
        return padding + (x - minX) * (this.canvas.width - 2 * padding) / (maxX - minX);
    }
    
    // Scale Y coordinate based on data range
    scaleY(y) {
        const padding = 50;
        const minY = Math.min(...this.cities.map(c => c.y));
        const maxY = Math.max(...this.cities.map(c => c.y));
        
        if (minY === maxY) return this.canvas.height / 2;
        
        return this.canvas.height - (padding + (y - minY) * (this.canvas.height - 2 * padding) / (maxY - minY));
    }
    
    // Inverse scale X for mouse interaction
    inverseScaleX(x) {
        const padding = 50;
        const minX = Math.min(...this.cities.map(c => c.x));
        const maxX = Math.max(...this.cities.map(c => c.x));
        
        if (minX === maxX) return minX;
        
        return minX + (x - padding) * (maxX - minX) / (this.canvas.width - 2 * padding);
    }
    
    // Inverse scale Y for mouse interaction
    inverseScaleY(y) {
        const padding = 50;
        const minY = Math.min(...this.cities.map(c => c.y));
        const maxY = Math.max(...this.cities.map(c => c.y));
        
        if (minY === maxY) return minY;
        
        return minY + (this.canvas.height - y - padding) * (maxY - minY) / (this.canvas.height - 2 * padding);
    }
    
    // Start animation
    startAnimation() {
        if (this.animationFrame) cancelAnimationFrame(this.animationFrame);
        this.isPaused = false;
        this.animate();
    }
    
    // Animation loop
    animate() {
        if (this.isPaused) return;
        
        this.draw();
        
        this.animationFrame = requestAnimationFrame(() => {
            setTimeout(() => this.animate(), this.animationSpeed);
        });
    }
    
    // Pause animation
    pauseAnimation() {
        this.isPaused = true;
        if (this.animationFrame) {
            cancelAnimationFrame(this.animationFrame);
            this.animationFrame = null;
        }
    }
    
    // Resume animation
    resumeAnimation() {
        if (this.isPaused) {
            this.isPaused = false;
            this.animate();
        }
    }
    
    // Reset visualization
    reset() {
        this.currentPath = [];
        this.bestPath = [];
        this.historicalPaths = [];
        this.historicalFitness = [];
        this.bestDistance = Infinity;
        this.currentDistance = 0;
        this.currentGeneration = 0;
        this.generationStats = [];
        
        if (this.animationFrame) {
            cancelAnimationFrame(this.animationFrame);
            this.animationFrame = null;
        }
        
        this.draw();
    }
    
    // Toggle historical paths display
    toggleHistoricalPaths() {
        this.showHistoricalPaths = !this.showHistoricalPaths;
        this.draw();
    }
    
    // Toggle heatmap display
    toggleHeatmap() {
        this.heatmap = !this.heatmap;
        this.draw();
    }
    
    // Set animation speed
    setAnimationSpeed(speed) {
        this.animationSpeed = speed;
    }
}

// Utility function to generate random cities
function generateRandomCities(count) {
    const cities = [];
    for (let i = 0; i < count; i++) {
        cities.push({
            name: `City ${i+1}`,
            x: Math.floor(Math.random() * 800),
            y: Math.floor(Math.random() * 600)
        });
    }
    return cities;
}

// Create fitness chart using Chart.js
function createFitnessChart(chartId) {
    const ctx = document.getElementById(chartId).getContext('2d');
    return new Chart(ctx, {
        type: 'line',
        data: {
            labels: [],
            datasets: [{
                label: 'Best Fitness',
                data: [],
                borderColor: '#2ecc71',
                backgroundColor: 'rgba(46, 204, 113, 0.1)',
                tension: 0.4,
                fill: true
            }, {
                label: 'Average Fitness',
                data: [],
                borderColor: '#3498db',
                backgroundColor: 'rgba(52, 152, 219, 0.1)',
                tension: 0.4,
                fill: true
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                legend: {
                    position: 'top',
                },
                tooltip: {
                    mode: 'index',
                    intersect: false,
                }
            },
            scales: {
                y: {
                    beginAtZero: true,
                    title: {
                        display: true,
                        text: 'Fitness'
                    }
                },
                x: {
                    title: {
                        display: true,
                        text: 'Generation'
                    }
                }
            }
        }
    });
}

// Update fitness chart with new data
function updateFitnessChart(chart, generation, bestFitness, avgFitness) {
    chart.data.labels.push(generation);
    chart.data.datasets[0].data.push(bestFitness);
    chart.data.datasets[1].data.push(avgFitness);
    
    // Limit chart data to keep performance
    if (chart.data.labels.length > 100) {
        chart.data.labels.shift();
        chart.data.datasets[0].data.shift();
        chart.data.datasets[1].data.shift();
    }
    
    chart.update();
}

// Export the visualization class and utilities
window.TSPVisualization = TSPVisualization;
window.generateRandomCities = generateRandomCities;
window.createFitnessChart = createFitnessChart;
window.updateFitnessChart = updateFitnessChart; 