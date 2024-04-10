function Image (element)
    if element.caption and element.attr.identifier then
      local num = element.attr.identifier:match("fig:(%d+%-%d+)")
      if num then
        local prefix = "Figure " .. num .. ": "
        element.caption[1].text = prefix .. element.caption[1].text
      end
    end
    return element
  end