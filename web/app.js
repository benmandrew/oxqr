// Load js_of_ocaml module
let generateQR = null;

async function initWasm() {
    try {
        // Load the js_of_ocaml compiled script
        const script = document.createElement('script');
        script.src = '../_build/default/bin/wasm.bc.js';
        return new Promise((resolve, reject) => {
            script.onload = () => {
                // The module exports are available on the global object
                if (window.generateQR) {
                    generateQR = window.generateQR;
                    resolve(true);
                } else {
                    reject(new Error('generateQR not found in module'));
                }
            };
            script.onerror = () => reject(new Error('Failed to load oxqr.js'));
            document.head.appendChild(script);
        });
    } catch (error) {
        console.error('Failed to load module:', error);
        return false;
    }
}

function showError(message) {
    const errorEl = document.getElementById('errorMessage');
    errorEl.textContent = message;
    errorEl.classList.add('visible');
}

function hideError() {
    const errorEl = document.getElementById('errorMessage');
    errorEl.classList.remove('visible');
}

function showLoading(show) {
    const loadingEl = document.getElementById('loading');
    if (show) {
        loadingEl.classList.add('visible');
    } else {
        loadingEl.classList.remove('visible');
    }
}

function displayQR(svgString) {
    const output = document.getElementById('output');
    const qrDisplay = document.getElementById('qrDisplay');
    // Inject SVG directly
    qrDisplay.innerHTML = svgString;
    output.classList.add('visible');
}

document.getElementById('qrForm').addEventListener('submit', async (e) => {
    e.preventDefault();
    hideError();
    const data = document.getElementById('data').value.trim();
    const ecl = document.getElementById('ecl').value;
    if (!data) {
        showError('Please enter some data to encode');
        return;
    }
    showLoading(true);
    try {
        // Initialize module if not already done
        if (!generateQR) {
            const success = await initWasm();
            if (!success) {
                throw new Error('Module not available');
            }
        }
        // Call the SVG export function
        const svgString = generateQR(data, ecl);
        displayQR(svgString);
    } catch (error) {
        showError('Error generating QR code: ' + error.message);
        console.error(error);
    } finally {
        showLoading(false);
    }
});

document.getElementById('qrForm').addEventListener('reset', () => {
    document.getElementById('output').classList.remove('visible');
    hideError();
});

// Initialize module on page load
window.addEventListener('load', () => {
    initWasm().catch(err => console.warn('Module init delayed:', err));
});
