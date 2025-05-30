# Vibe Project Skeleton
# This file can contain template code that gets copied to new projects

print("Vibe Project Template Loaded!")

# Example function
create_vibe <- function(intensity = "moderate") {
  vibes <- list(
    low = "🎵 Chill vibes only",
    moderate = "🎶 Good vibes flowing",
    high = "🎸 Maximum vibe energy!"
  )

  message(vibes[[intensity]])
}

# Call the function
create_vibe()
