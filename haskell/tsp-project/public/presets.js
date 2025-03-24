/**
 * City Presets Manager
 * Handles loading and managing city presets for the TSP application
 */

class TSPPresets {
    constructor() {
        this.presets = {
            usa: null,
            europe: null,
            grid: null,
            capitals: null,
            custom: []
        };
        
        this.loadAllPresets();
    }
    
    /**
     * Load all available presets
     */
    async loadAllPresets() {
        try {
            const [usa, europe] = await Promise.all([
                this.loadPreset('usa'),
                this.loadPreset('europe')
            ]);
            
            this.presets.usa = usa;
            this.presets.europe = europe;
            
            // Generate grid preset
            this.presets.grid = this.generateGridPreset(5, 5, 100);
            
            // Generate world capitals
            this.presets.capitals = this.generateCapitalsPreset();
            
            console.log('All presets loaded successfully');
        } catch (error) {
            console.error('Error loading presets:', error);
        }
    }
    
    /**
     * Load a single preset by name
     */
    async loadPreset(name) {
        try {
            const response = await fetch(`presets/${name}.json`);
            if (!response.ok) {
                throw new Error(`Error loading preset ${name}: ${response.statusText}`);
            }
            return await response.json();
        } catch (error) {
            console.error(`Failed to load preset ${name}:`, error);
            return null;
        }
    }
    
    /**
     * Get a preset by name
     */
    getPreset(name) {
        if (this.presets[name]) {
            return [...this.presets[name]]; // Return a copy
        }
        return null;
    }
    
    /**
     * Save a custom preset
     */
    saveCustomPreset(name, cities) {
        this.presets.custom.push({
            name: name,
            cities: [...cities] // Save a copy
        });
    }
    
    /**
     * Get all custom presets
     */
    getCustomPresets() {
        return this.presets.custom;
    }
    
    /**
     * Get all available preset names
     */
    getPresetNames() {
        const builtInPresets = Object.keys(this.presets).filter(key => key !== 'custom');
        const customPresets = this.presets.custom.map(preset => preset.name);
        return [...builtInPresets, ...customPresets];
    }
    
    /**
     * Generate a grid preset with given dimensions
     */
    generateGridPreset(rows, cols, spacing) {
        const cities = [];
        let index = 1;
        
        for (let i = 0; i < rows; i++) {
            for (let j = 0; j < cols; j++) {
                cities.push({
                    name: `G${index}`,
                    x: j * spacing + 100,
                    y: i * spacing + 100
                });
                index++;
            }
        }
        
        return cities;
    }
    
    /**
     * Generate world capitals preset
     */
    generateCapitalsPreset() {
        return [
            {"name": "Washington DC", "x": 280, "y": 220},
            {"name": "Ottawa", "x": 320, "y": 180},
            {"name": "Mexico City", "x": 230, "y": 310},
            {"name": "Brasilia", "x": 350, "y": 400},
            {"name": "Buenos Aires", "x": 320, "y": 450},
            {"name": "London", "x": 470, "y": 180},
            {"name": "Paris", "x": 480, "y": 190},
            {"name": "Berlin", "x": 500, "y": 180},
            {"name": "Rome", "x": 500, "y": 220},
            {"name": "Moscow", "x": 550, "y": 160},
            {"name": "Beijing", "x": 680, "y": 220},
            {"name": "Tokyo", "x": 750, "y": 230},
            {"name": "New Delhi", "x": 620, "y": 270},
            {"name": "Canberra", "x": 750, "y": 450},
            {"name": "Pretoria", "x": 520, "y": 400},
            {"name": "Cairo", "x": 530, "y": 250},
            {"name": "Riyadh", "x": 560, "y": 270},
            {"name": "Jakarta", "x": 680, "y": 350},
            {"name": "Seoul", "x": 720, "y": 220},
            {"name": "Bangkok", "x": 660, "y": 300}
        ];
    }
}

// Initialize and make globally available
window.tspPresets = new TSPPresets(); 