document.addEventListener("click", function (event) {
  var th = event.target.closest(".data-output-wrap table thead th");
  if (!th) {
    return;
  }

  var table = th.closest("table");
  var tbody = table.querySelector("tbody");
  if (!tbody) {
    return;
  }

  var headers = Array.from(table.querySelectorAll("thead th"));
  var index = headers.indexOf(th);
  var rows = Array.from(tbody.querySelectorAll("tr"));
  var numeric = rows.some(function (row) {
    var text = cellText(row, index);
    return text !== "" && !isNaN(Number(text));
  });

  rows.forEach(function (row, i) {
    if (!row.dataset.originalIndex) {
      row.dataset.originalIndex = String(i);
    }
  });

  var first = numeric ? "desc" : "asc";
  var second = numeric ? "asc" : "desc";
  var next;
  if (th.dataset.sort === first) {
    next = second;
  } else if (th.dataset.sort === second) {
    next = "";
  } else {
    next = first;
  }

  headers.forEach(function (header) {
    header.dataset.sort = "";
    header.classList.remove("sort-asc", "sort-desc");
  });
  th.dataset.sort = next;
  if (next === "asc" || next === "desc") {
    th.classList.add(next === "asc" ? "sort-asc" : "sort-desc");
  }

  if (next === "") {
    rows.sort(function (a, b) {
      return Number(a.dataset.originalIndex) - Number(b.dataset.originalIndex);
    });
  } else {
    rows.sort(function (a, b) {
      return compareCells(cellText(a, index), cellText(b, index), next === "asc");
    });
  }
  rows.forEach(function (row) {
    tbody.appendChild(row);
  });
});

function cellText(row, index) {
  var cell = row.children[index];
  return cell ? cell.textContent.trim() : "";
}

function compareCells(aText, bText, ascending) {
  if (aText === "" && bText !== "") {
    return 1;
  }
  if (bText === "" && aText !== "") {
    return -1;
  }

  var aNum = Number(aText);
  var bNum = Number(bText);
  var cmp;
  if (aText !== "" && bText !== "" && !isNaN(aNum) && !isNaN(bNum)) {
    cmp = aNum - bNum;
  } else {
    cmp = aText.localeCompare(bText, undefined, { numeric: true, sensitivity: "base" });
  }
  return ascending ? cmp : -cmp;
}
