const repoGrid = document.querySelector("#repo-grid");

const fallbackRepos = [
  {
    name: "Reusable civic tech starter",
    html_url: "https://github.com/orgs/Steal-This-Code/repositories",
    description: "Add your first public project to replace this placeholder once the organization repos are live.",
    language: "Open Source",
    stargazers_count: 0,
    updated_at: "2026-04-27T00:00:00Z"
  }
];

function escapeHtml(value) {
  return String(value).replace(/[&<>"']/g, (character) => {
    const entities = {
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#39;"
    };

    return entities[character];
  });
}

function formatDate(isoDate) {
  return new Intl.DateTimeFormat("en-US", {
    month: "short",
    day: "numeric",
    year: "numeric"
  }).format(new Date(isoDate));
}

function renderRepos(repos) {
  repoGrid.innerHTML = repos
    .map((repo) => {
      const description =
        repo.description ||
        "A public project from the Steal This Code organization.";
      const safeName = escapeHtml(repo.name);
      const safeDescription = escapeHtml(description);
      const safeLanguage = escapeHtml(repo.language || "Repository");
      const safeUrl = escapeHtml(repo.html_url);

      return `
        <article class="repo-card">
          <span class="repo-tag">${safeLanguage}</span>
          <a href="${safeUrl}">
            <h3>${safeName}</h3>
          </a>
          <p>${safeDescription}</p>
          <div class="repo-meta">
            <span>Updated ${formatDate(repo.updated_at)}</span>
            <span>${repo.stargazers_count || 0} stars</span>
          </div>
        </article>
      `;
    })
    .join("");
}

async function loadRepos() {
  try {
    const response = await fetch(
      "https://api.github.com/orgs/Steal-This-Code/repos?per_page=6&sort=updated"
    );

    if (!response.ok) {
      throw new Error(`GitHub API responded with ${response.status}`);
    }

    const repos = await response.json();
    const publicRepos = repos
      .filter((repo) => !repo.fork)
      .sort((a, b) => new Date(b.pushed_at) - new Date(a.pushed_at))
      .slice(0, 6);

    renderRepos(publicRepos.length ? publicRepos : fallbackRepos);
  } catch (error) {
    renderRepos(fallbackRepos);
  }
}

loadRepos();
