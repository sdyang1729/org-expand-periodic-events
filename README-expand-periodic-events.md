# org-expand-periodic-events.el

Expand periodic recurring events into individual one-time event entries.

## What It Does

Converts this:
```org
** Weekly Meeting
<2025-11-04 Wed 14:00-15:00 +1w>--<2025-12-25 Thu>
```

Into this:
```org
** Weekly Meeting
<2025-11-04 Wed 14:00-15:00>
** Weekly Meeting
<2025-11-11 Wed 14:00-15:00>
** Weekly Meeting
<2025-11-18 Wed 14:00-15:00>
** Weekly Meeting
<2025-11-25 Wed 14:00-15:00>
** Weekly Meeting
<2025-12-02 Wed 14:00-15:00>
** Weekly Meeting
<2025-12-09 Wed 14:00-15:00>
** Weekly Meeting
<2025-12-16 Wed 14:00-15:00>
** Weekly Meeting
<2025-12-23 Wed 14:00-15:00>
```

## Installation

Add to your Emacs config:

```elisp
(load-file "~/path/to/org-expand-periodic-events.el")
```

Or if it's in your load-path:

```elisp
(require 'org-expand-periodic-events)
```

## Usage

### Method 1: Expand Event at Point

1. Write your periodic event:
   ```org
   ** Team Standup
   <2025-11-04 Wed 14:00-15:00 +1w>--<2025-12-25 Thu>
   ```

2. Place cursor on the headline or timestamp line

3. Run: `M-x org-expand-periodic-event-at-point`

4. The entry will be replaced with individual events for each occurrence

### Method 2: Expand All Events in Buffer

Run: `M-x org-expand-periodic-events-in-buffer`

This will find and expand ALL periodic events in the current buffer.

### Method 3: Interactive Insert

Run: `M-x org-expand-insert-expanded-event`

Then answer the prompts:
```
Headline: Weekly Meeting
Start date (YYYY-MM-DD): 2025-11-04
End date (YYYY-MM-DD): 2025-12-25
Time (HH:MM-HH:MM, optional): 14:00-15:00
Repeater (+1w/+2w/+1d, optional): +1w
```

This will insert all the individual events at point.

### Method 4: Generate to Kill Ring

Run: `M-x org-expand-periodic-event`

Enter the timestamps and the result will be copied to the kill ring, ready to paste.

## Supported Repeaters

- `+1w` - Weekly (every 7 days)
- `+2w` - Bi-weekly (every 14 days)
- `+1d` - Daily
- `+1m` - Monthly (approximately)
- `+1y` - Yearly

## Examples

### Example 1: Daily Conference

**Input:**
```org
** AI Conference
<2025-11-10 Mon 09:00-17:00 +1d>--<2025-11-14 Fri>
```

**After expansion:**
```org
** AI Conference
<2025-11-10 Mon 09:00-17:00>
** AI Conference
<2025-11-11 Tue 09:00-17:00>
** AI Conference
<2025-11-12 Wed 09:00-17:00>
** AI Conference
<2025-11-13 Thu 09:00-17:00>
** AI Conference
<2025-11-14 Fri 09:00-17:00>
```

### Example 2: Bi-weekly Meeting

**Input:**
```org
** Sprint Planning
<2025-11-04 Wed 10:00-11:00 +2w>--<2025-12-25 Thu>
```

**After expansion:**
```org
** Sprint Planning
<2025-11-04 Wed 10:00-11:00>
** Sprint Planning
<2025-11-18 Wed 10:00-11:00>
** Sprint Planning
<2025-12-02 Wed 10:00-11:00>
** Sprint Planning
<2025-12-16 Wed 10:00-11:00>
```

### Example 3: All-day Events

**Input:**
```org
** Vacation
<2025-12-20 Sat +1d>--<2025-12-27 Sat>
```

**After expansion:**
```org
** Vacation
<2025-12-20 Sat>
** Vacation
<2025-12-21 Sun>
** Vacation
<2025-12-22 Mon>
** Vacation
<2025-12-23 Tue>
** Vacation
<2025-12-24 Wed>
** Vacation
<2025-12-25 Thu>
** Vacation
<2025-12-26 Fri>
** Vacation
<2025-12-27 Sat>
```

## Why Use This?

1. **Simplicity**: Each event is a simple, standalone timestamp
2. **Compatibility**: Works perfectly with calfw-org, org-agenda, and all org-mode features
3. **Flexibility**: Easy to modify individual occurrences (cancel one meeting, change time, etc.)
4. **No sexp complexity**: No need to deal with diary sexp syntax
5. **Visual clarity**: See all your events listed explicitly

## Integration with org-roam and calfw

These expanded events work seamlessly with:
- `org-agenda`
- `calfw-org`
- `org-roam` daily notes
- Any org-mode calendar tool

Just make sure your org file is in `org-agenda-files`:

```elisp
(setq org-agenda-files '("~/org-roam/calendar.org"))
```

## Keybindings (Optional)

Add to your config:

```elisp
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e e") 'org-expand-periodic-event-at-point)
  (define-key org-mode-map (kbd "C-c e b") 'org-expand-periodic-events-in-buffer)
  (define-key org-mode-map (kbd "C-c e i") 'org-expand-insert-expanded-event))
```

## Tips

1. **Before expanding**: Make sure you're happy with the date range - expanding creates many entries!

2. **Undo works**: If you expand by mistake, just undo (`C-/`)

3. **Best practice**: Use this for events with a definite end date. For ongoing events (like a weekly meeting that continues indefinitely), consider using a simple repeater: `<2025-11-04 Wed 14:00-15:00 +1w>`

4. **Archive old events**: After events pass, you can archive them with `C-c C-x a`

## Comparison with Sexp Approach

| Feature | Expanded Events | Sexp Events |
|---------|----------------|-------------|
| Easy to edit individual events | ✓ | ✗ |
| Works with all org features | ✓ | ~ |
| Compact representation | ✗ | ✓ |
| Easy to understand | ✓ | ✗ |
| Can modify single occurrence | ✓ | ✗ |

Choose expanded events when you want maximum flexibility and clarity. Choose sexp when you have many recurring events and want compact representation.

## License

Public Domain / Unlicense
