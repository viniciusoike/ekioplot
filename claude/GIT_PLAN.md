# Git Version Control Plan for ekioplot v2.0

## Overview

This document outlines the recommended git workflow for the v2.0 refactoring.
The changes are structured into logical commits to maintain a clean, reviewable history.

---

## Recommended Commit Strategy

### Option A: Single Feature Branch (Recommended)

Create a feature branch, make atomic commits, then merge to master.

```bash
# Create feature branch
git checkout -b feature/v2.0-refactor

# Make commits (see below)
# ...

# Merge to master when ready
git checkout master
git merge feature/v2.0-refactor

# Tag the release
git tag -a v2.0.0 -m "EKIO Design System v2.0"
git push origin master --tags
```

### Option B: Direct to Master (if working solo)

Make atomic commits directly to master, then tag.

---

## Commit Plan (Atomic Commits)

### Commit 1: Add new color system
```bash
git add R/colors.R R/external_palettes.R
git commit -m "feat: Add new EKIO color system v2.0

- Add ekio_pal() for qualitative palettes (7 palettes)
- Add ekio_seq_pal() for sequential palettes (4 palettes)
- Add ekio_div_pal() for diverging palettes (3 palettes)
- Add color scale vectors (ekio_blue, ekio_gray, ekio_teal, ekio_orange)
- Separate external palettes (viridis, okabe_ito) into dedicated file
- Clear distinction between EKIO and non-EKIO palettes"
```

### Commit 2: Add new scales and theme
```bash
git add R/scales.R R/theme.R
git commit -m "feat: Add simplified scales and theme

- Add scale_*_ekio_d() for discrete scales
- Add scale_*_ekio_c() for continuous scales
- Add scale_*_ekio_div() for diverging scales (NEW)
- Simplify theme_ekio() with grid parameter
- Add theme_ekio_map() and theme_ekio_presentation()
- Remove 6 style variants"
```

### Commit 3: Add aesthetic detection
```bash
git add R/detect_aesthetic.R
git commit -m "feat: Add smart aesthetic detection

- Add detect_aesthetic_type() to distinguish color string vs variable
- Add is_valid_color() helper
- Add warn_palette_ignored() for user education
- Enables intuitive API: fill = 'blue' OR fill = category"
```

### Commit 4: Update recipes with aesthetic detection
```bash
git add R/ekio_recipes.R
git commit -m "refactor: Update plot recipes with aesthetic detection

- ekio_histogram() now auto-detects fill type
- ekio_barplot() now auto-detects fill type
- ekio_lineplot() now auto-detects color type
- ekio_scatterplot() now auto-detects color type
- Remove old palette/style parameters"
```

### Commit 5: Simplify supporting files
```bash
git add R/fonts.R R/gt_theme_ekio.R R/show_palette.R R/utils.R
git commit -m "refactor: Simplify fonts, GT themes, and utilities

- Simplify font management (remove extrafont/showtext/sysfonts)
- Simplify GT themes (remove style variants)
- Add show_ekio_palette() and show_all_ekio_palettes()
- Clean up utils.R to imports only"
```

### Commit 6: Remove deprecated files
```bash
git add -u  # stages deletions
git commit -m "chore: Remove deprecated files

BREAKING CHANGE: Old API removed

Deleted files:
- R/ekio_colors.R (replaced by R/colors.R)
- R/ekio_palette.R (merged into R/colors.R)
- R/ekio_scales.R (replaced by R/scales.R)
- R/ekio_main_theme.R (replaced by R/theme.R)"
```

### Commit 7: Update package configuration
```bash
git add DESCRIPTION
git commit -m "chore: Update DESCRIPTION for v2.0.0

- Bump version to 2.0.0
- Update title and description
- Remove Imports: extrafont, showtext, sysfonts, ggbump, ggridges, tidyr
- Add grDevices to Imports"
```

### Commit 8: Add development documentation
```bash
git add claude/
git commit -m "docs: Add development documentation

- Add EKIOPLOT_UPDATE_INSTRUCTIONS.md (marked outdated)
- Add GIT_PLAN.md
- Add example_aesthetic.R reference"
```

---

## Quick Single Commit Alternative

If you prefer a single commit for the entire refactor:

```bash
git add -A
git commit -m "feat!: EKIO Design System v2.0 - Major Refactoring

BREAKING CHANGES:
- Consolidated 42+ palettes to ~15 core palettes
- Removed theme style variants (modern_premium, academic_authority, etc.)
- Removed indices parameter from scales
- Changed ekio_colors() to ekio_pal()/ekio_seq_pal()/ekio_div_pal()
- Removed font management dependencies

New features:
- Smart aesthetic detection (fill = 'blue' OR fill = variable)
- Diverging scales (scale_*_ekio_div)
- External palettes clearly separated (viridis, okabe_ito)
- Simplified theme with grid parameter
- New palette visualization utilities

Files added:
- R/colors.R, R/external_palettes.R, R/scales.R
- R/theme.R, R/detect_aesthetic.R, R/show_palette.R

Files removed:
- R/ekio_colors.R, R/ekio_palette.R
- R/ekio_scales.R, R/ekio_main_theme.R"
```

---

## Post-Commit Steps

1. **Run package checks**
   ```r
   devtools::document()
   devtools::check()
   ```

2. **Tag the release**
   ```bash
   git tag -a v2.0.0 -m "EKIO Design System v2.0 - Breaking Changes"
   ```

3. **Push to remote**
   ```bash
   git push origin master --tags
   ```

4. **Create GitHub Release** (optional)
   - Go to GitHub releases
   - Create new release from v2.0.0 tag
   - Add release notes

---

## Rollback Plan

If issues arise, rollback to previous state:

```bash
# View commit history
git log --oneline

# Revert to specific commit
git revert <commit-hash>

# Or reset to tag (destructive)
git reset --hard v0.1.0
```
