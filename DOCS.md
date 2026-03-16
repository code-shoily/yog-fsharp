# Documentation Guide

This document explains how to build and deploy the Yog.FSharp documentation.

## 📚 Documentation Structure

```
docs/
├── index.md                          # Main landing page (uses README content)
├── examples.md                       # Examples index page
├── tutorials.md                      # Tutorials index page
├── _template.html                    # Custom HTML template
├── examples/                         # Example F# scripts (37 files)
│   ├── gps-navigation.fsx
│   ├── bridges-of-konigsberg.fsx
│   ├── dijkstra-shortest-path.fsx
│   ├── social-network-analysis.fsx
│   ├── centrality-analysis.fsx
│   ├── network-bandwidth.fsx
│   ├── dag-algorithms.fsx
│   ├── render-dot.fsx
│   ├── render-graphml.fsx
│   ├── render-json.fsx
│   └── ... (27 more examples)
└── tutorials/                        # Tutorial documentation
    └── getting-started.md
```

## 🔧 Building Documentation Locally

### Prerequisites

- .NET SDK 10.0.103 or later
- FSDocs tool (automatically restored via dotnet-tools.json)

### Build Commands

```bash
# Restore tools
dotnet tool restore

# Build documentation (output to ./output/)
dotnet fsdocs build --clean

# Build and watch for changes (runs local server)
dotnet fsdocs watch

# Build for production (with custom parameters)
dotnet fsdocs build --clean \
  --properties Configuration=Release \
  --parameters fsdocs-authors "Mafinar K" \
  fsdocs-repository-link "https://github.com/code-shoily/yog-fsharp" \
  root "/yog-fsharp/"
```

### Preview Locally

After building, preview the documentation:

```bash
# Option 1: Use fsdocs watch (recommended)
dotnet fsdocs watch
# Opens browser at http://localhost:8901

# Option 2: Use any HTTP server
cd output
python3 -m http.server 8080
# Open http://localhost:8080
```

## 🚀 GitHub Pages Deployment

### Automatic Deployment

The documentation is automatically built and deployed to GitHub Pages on every push to `main`.

**Workflow:** `.github/workflows/docs.yml`

**Deployment URL:** https://code-shoily.github.io/yog-fsharp/

### Manual Deployment

If you need to deploy manually:

```bash
# Build docs
dotnet fsdocs build --clean --properties Configuration=Release

# The output/ directory contains the complete static site
# Deploy this to any static hosting service
```

## 📝 Adding New Documentation

### Adding Examples

1. **Create F# script**:
   ```bash
   # Create in docs/examples/ folder
   vi docs/examples/my-example.fsx
   ```

2. **Update examples index**:
   ```bash
   # Add link in docs/examples.md
   vi docs/examples.md
   ```

3. **Build and preview**:
   ```bash
   dotnet fsdocs watch
   # Opens browser at http://localhost:8901
   ```

**Note:** FSDocs automatically generates HTML from `.fsx` files with syntax highlighting and execution support. The script files serve both as documentation and downloadable examples.

### Adding Tutorials

1. **Create tutorial page**:
   ```bash
   vi docs/tutorials/my-tutorial.md
   ```

2. **Update tutorials index**:
   ```bash
   vi docs/tutorials.md
   ```

## 🎨 Customizing Appearance

### Template Customization

The documentation uses a custom template at `docs/_template.html`.

**Key sections to customize:**
- CSS variables in `<style>` block
- Navigation menu structure
- Header/footer content

### Styling

Customize colors and fonts in `docs/_template.html`:

```css
:root {
  --main-color: #4A90E2;        /* Primary brand color */
  --accent-color: #50C878;      /* Accent/highlight color */
  --text-color: #333;           /* Main text */
  --bg-color: #fafafa;          /* Background */
  --code-bg: #f4f4f4;           /* Code blocks */
}
```

## 📖 Writing Documentation

### Markdown Features

FSDocs supports GitHub-flavored markdown with F# syntax highlighting:

````markdown
# Heading

Some text with **bold** and *italic*.

```fsharp
open Yog.Model

let graph = empty Directed
```

## Tables

| Feature | Supported |
|---------|-----------|
| Tables  | ✓         |
| Math    | ✗         |
````

### API Documentation

API reference is automatically generated from XML documentation comments in source code:

```fsharp
/// Finds the shortest path using Dijkstra's algorithm.
///
/// ## Parameters
/// - `zero`: The zero element for weights
/// - `graph`: The input graph
///
/// ## Returns
/// `Some path` if found, `None` otherwise
let shortestPath zero add compare start goal graph =
    // ...
```

## 🔍 Troubleshooting

### Documentation not building

```bash
# Clean and rebuild
rm -rf output .fsdocs
dotnet fsdocs build --clean
```

### API reference not showing

Ensure `GenerateDocumentationFile` is enabled in your .fsproj:

```xml
<PropertyGroup>
  <GenerateDocumentationFile>true</GenerateDocumentationFile>
</PropertyGroup>
```

### GitHub Pages not updating

1. Check GitHub Actions workflow status
2. Ensure GitHub Pages is enabled in repository settings
3. Verify deployment branch is set to `gh-pages`

## 📦 Output Structure

After building, the `output/` directory contains:

```
output/
├── index.html                    # Landing page
├── examples.html                 # Examples index
├── tutorials.html                # Tutorials index
├── examples/                     # Example pages
│   ├── gps-navigation.html
│   └── ...
├── tutorials/                    # Tutorial pages
│   ├── getting-started.html
│   └── ...
├── reference/                    # API reference (auto-generated)
│   ├── index.html
│   ├── yog-model.html
│   └── ...
└── content/                      # Assets (CSS, JS, images)
    ├── fsdocs-default.css
    ├── fsdocs-search.js
    └── img/
```

## 📋 Current Examples (37 total)

### Pathfinding & Algorithms
- `dijkstra-shortest-path.fsx` - Shortest path using Dijkstra
- `bellman-ford.fsx` - Bellman-Ford algorithm
- `gps-navigation.fsx` - GPS navigation example
- `city-distance-matrix.fsx` - Distance matrix calculations
- `centrality-analysis.fsx` - Network centrality measures

### Graph Generators
- `complete-graphs.fsx` - Complete graph generation
- `cycle-graphs.fsx` - Cycle graph generation
- `path-graphs.fsx` - Path graph generation
- `star-graphs.fsx` - Star graph generation
- `wheel-graphs.fsx` - Wheel graph generation
- `grid-graphs.fsx` - Grid graph generation
- `petersen-graph.fsx` - Petersen graph
- `binary-trees.fsx` - Binary tree generation
- `empty-graphs.fsx` - Empty graph creation
- `bipartite-graphs.fsx` - Bipartite graph generation
- `random-trees.fsx` - Random tree generation

### Random Graph Models
- `erdos-renyi-graphs.fsx` - Erdős-Rényi G(n,p) model
- `erdos-renyi-gnm-graphs.fsx` - Erdős-Rényi G(n,m) model
- `barabasi-albert-networks.fsx` - Barabási-Albert scale-free networks
- `watts-strogatz-networks.fsx` - Watts-Strogatz small-world networks

### Flow & Optimization
- `network-bandwidth.fsx` - Maximum flow/bandwidth
- `network-cable-layout.fsx` - Minimum spanning tree
- `global-min-cut.fsx` - Global minimum cut

### Graph Properties
- `bridges-of-konigsberg.fsx` - Eulerian paths/circuits
- `bridges-and-articulation.fsx` - Bridge and articulation point detection
- `clique-finding.fsx` - Maximum clique detection
- `social-network-analysis.fsx` - Social network properties

### DAG & Scheduling
- `dag-algorithms.fsx` - Topological sorting and DAG algorithms
- `task-ordering.fsx` - Task dependency ordering
- `task-scheduling.fsx` - Task scheduling with constraints
- `cave-path-counting.fsx` - Path counting in DAGs

### Matching & Assignment
- `job-matching.fsx` - Bipartite matching
- `job-assignment.fsx` - Assignment problem
- `medical-residency.fsx` - Stable marriage (Gale-Shapley)

### Visualization & I/O
- `render-dot.fsx` - DOT format export
- `render-graphml.fsx` - GraphML format export
- `render-gdf.fsx` - GDF format export
- `render-json.fsx` - JSON format export

## 📋 Project Files

- **CHANGELOG.md** - Version history and release notes
- **README.md** - Project overview (synced to docs/index.md)
- **DOCS.md** - This documentation guide
- **docs/GLEAM_FSHARP_COMPARISON.md** - Detailed comparison with Gleam version
- **NETWORK_SIMPLEX_ISSUES.md** - Known issues with Network Simplex algorithm
- **GLEAM_NETWORK_SIMPLEX_ANALYSIS.md** - Analysis of Gleam's complete implementation
- **LICENSE** - MIT License

## 🌐 External Resources

- [FSDocs Documentation](https://fsprojects.github.io/FSharp.Formatting/)
- [GitHub Pages Setup](https://docs.github.com/en/pages)
- [Markdown Guide](https://www.markdownguide.org/)

---

**Questions?** Open an issue on [GitHub](https://github.com/code-shoily/yog-fsharp/issues)
