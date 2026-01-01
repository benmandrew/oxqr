# OXQR Web Interface

A simple webpage for generating QR codes using the OXQR WebAssembly module.

## Setup

1. **Build the WASM module:**
   ```bash
   dune build bin/wasm.bc.js
   ```

2. **Serve the web directory:**
   ```bash
   # Using Python 3
   python3 -m http.server --directory web 8000
   
   # Or using Node.js (http-server)
   npx http-server web -p 8000
   ```

3. **Open in browser:**
   Navigate to `http://localhost:8000`

## Features

- **Simple UI**: Clean, modern interface for QR code generation
- **Error correction levels**: Choose between L, M, Q, H levels
- **Real-time generation**: Instant QR code display
- **Responsive design**: Works on desktop and mobile
- **No dependencies**: Pure HTML/CSS/JavaScript + WebAssembly

## Usage

1. Enter alphanumeric text (A-Z, 0-9, space, `$%*+-./:`)
2. Select an error correction level (default: M)
3. Click "Generate QR Code"
4. The QR code appears as Unicode block characters

## Supported Characters

- Letters: A-Z (uppercase only)
- Numbers: 0-9
- Special characters: space, `$`, `%`, `*`, `+`, `-`, `.`, `/`, `:`

## Building for Production

To deploy, build the WASM module and copy the generated files to your web server:

```bash
dune build --release bin/wasm.bc.js
cp _build/default/bin/wasm.* web/
```

## Browser Support

- Chrome/Edge 57+
- Firefox 52+
- Safari 11+
- Any modern browser with WebAssembly support
