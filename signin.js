function safeInitGoogleSignIn() {
  if (typeof google !== 'undefined' && google.accounts && google.accounts.id) {
    initGoogleSignIn();
  } else {
    setTimeout(safeInitGoogleSignIn, 500);
  }
}

function initGoogleSignIn() {
  google.accounts.id.initialize({
    client_id: "401792939422-isi34drqbakks9676em4aj1ipvh663g1.apps.googleusercontent.com",
    callback: handleCredentialResponse
  });
  renderSignInButton();
  google.accounts.id.prompt();
}

function renderSignInButton() {
  var signinDiv = document.getElementById("signin");
  if (signinDiv) {
    signinDiv.innerHTML = "";
    google.accounts.id.renderButton(
      signinDiv,
      { theme: "outline", size: "large" }
    );
  } else {
    setTimeout(renderSignInButton, 500);
  }
}

function handleCredentialResponse(response) {
  var jwt = response.credential;
  var user = parseJwt(jwt);
  Shiny.onInputChange('g.id', user.sub);
  Shiny.onInputChange('g.name', user.name);
  Shiny.onInputChange('g.email', user.email);
  Shiny.onInputChange('g.image', user.picture);
}

function parseJwt(token) {
  var base64Url = token.split('.')[1];
  var base64 = base64Url.replace(/-/g, '+').replace(/_/g, '/');
  var jsonPayload = decodeURIComponent(
    atob(base64)
      .split('')
      .map(function(c) {
        return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
      })
      .join('')
  );
  return JSON.parse(jsonPayload);
}

function signOut() {
  Shiny.onInputChange('g.id', null);
  Shiny.onInputChange('g.name', null);
  Shiny.onInputChange('g.email', null);
  Shiny.onInputChange('g.image', null);
  location.reload();
}

// Custom message handler so the server can trigger initialization.
Shiny.addCustomMessageHandler("init-signin", function(message) {
  safeInitGoogleSignIn();
});

// Fallback: if the login page loads immediately, try safe initialization.
window.onload = safeInitGoogleSignIn;
