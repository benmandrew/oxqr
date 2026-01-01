// Load js_of_ocaml module
let generateQR = null;
let currentDownloadUrl = null;
const ALPHANUMERIC_PATTERN = /^[0-9A-Z $%*+\-./:]+$/;

async function initWasm() {
    try {
        const mod = await import('../_build/default/bin/web.bc.js');
        if (typeof mod.default === 'function') {
            await mod.default();
        }
        if (window.generateQR) {
            generateQR = window.generateQR;
            return true;
        }
        throw new Error('generateQR not found in module');
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
    updateDownloadLink(svgString);
}

function clearOutput() {
    document.getElementById('output').classList.remove('visible');
    updateDownloadLink(null);
}

function isValidAlphanumeric(text) {
    return ALPHANUMERIC_PATTERN.test(text);
}

function updateDownloadLink(svgString) {
    const btn = document.getElementById('downloadBtn');
    // Revoke any previous object URL to avoid leaks
    if (currentDownloadUrl) {
        URL.revokeObjectURL(currentDownloadUrl);
        currentDownloadUrl = null;
    }
    if (!svgString) {
        btn.classList.remove('visible');
        btn.removeAttribute('href');
        btn.setAttribute('aria-disabled', 'true');
        return;
    }
    const blob = new Blob([svgString], { type: 'image/svg+xml;charset=utf-8' });
    currentDownloadUrl = URL.createObjectURL(blob);
    btn.href = currentDownloadUrl;
    btn.download = 'qr.svg';
    btn.classList.add('visible');
    btn.removeAttribute('aria-disabled');
}

// Generate QR code from current form values
async function generateQRFromInputs() {
    hideError();
    const data = document.getElementById('data').value.trim();
    const ecl = document.getElementById('ecl').value;
    if (!data) {
        clearOutput();
        return;
    }
    if (!isValidAlphanumeric(data)) {
        clearOutput();
        showError('Input must be alphanumeric (A-Z, 0-9, space, $%*+-./:)');
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
}

document.getElementById('qrForm').addEventListener('submit', async (e) => {
    e.preventDefault();
    await generateQRFromInputs();
});

// Auto-generate on input change
document.getElementById('data').addEventListener('input', () => {
    generateQRFromInputs();
});

// Auto-generate on ECL change
document.getElementById('ecl').addEventListener('change', () => {
    generateQRFromInputs();
});

document.getElementById('qrForm').addEventListener('reset', () => {
    clearOutput();
    hideError();
});

// Initialize module on page load and generate initial QR
window.addEventListener('load', async () => {
    try {
        await initWasm();
        // Generate QR with default value
        await generateQRFromInputs();
    } catch (err) {
        console.warn('Module init delayed:', err);
    }
});
