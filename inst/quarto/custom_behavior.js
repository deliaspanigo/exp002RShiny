document.addEventListener("DOMContentLoaded", function() {
  // 1️⃣ Lista con el orden que querés mostrar en pantalla
  const visualOrder = [
    "Report",
    "Hypothesis",
    "Requeriments",
    "Plots - Raw Data",
    "Plots - Residuals",
    "Script",
    "R Code and Outputs" // Querés que esta sea la última visualmente
  ];

  // 2️⃣ Pestaña que querés activar por defecto
  const defaultTab = "Report";

  setTimeout(function() {
    const tabsets = document.querySelectorAll('.panel-tabset');

    tabsets.forEach(tabset => {
      const nav = tabset.querySelector('.nav-tabs');
      const tabs = [...tabset.querySelectorAll('.nav-item')];

      // Reordenar los tabs según visualOrder
      visualOrder.forEach(name => {
        const tab = tabs.find(t => t.textContent.trim() === name);
        if (tab) nav.appendChild(tab);
      });

      // Activar la pestaña predeterminada
      const defaultTabLink = [...tabset.querySelectorAll('.nav-link')]
        .find(a => a.textContent.trim() === defaultTab);
      if (defaultTabLink) defaultTabLink.click();
    });
  }, 100);
});

