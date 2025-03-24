/**
 * Setup script for the TSP Genetic Algorithm project
 */
const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

console.log('🚀 Setting up the TSP Genetic Algorithm project...');

// Create necessary directories
const dirs = ['public/presets'];
dirs.forEach(dir => {
  if (!fs.existsSync(dir)) {
    console.log(`Creating directory: ${dir}`);
    fs.mkdirSync(dir, { recursive: true });
  }
});

// Try to build the Haskell project
try {
  console.log('Building Haskell project...');
  execSync('cabal build', { stdio: 'inherit' });
  console.log('✅ Haskell project built successfully');
} catch (error) {
  console.error('❌ Failed to build Haskell project. Is Cabal installed?');
  console.error('You can still run the project with the JavaScript fallback.');
}

// Create sample presets
const presetsDir = path.join(__dirname, 'public', 'presets');
console.log('Creating sample city presets...');

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

console.log('✅ Setup complete! You can now run:');
console.log('   npm start - to start the server');
console.log('   npm run dev - to start the server with auto-restart'); 