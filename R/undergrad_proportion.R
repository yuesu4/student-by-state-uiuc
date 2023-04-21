calc_undergrad_proportion = function(data) {
  data|>
    mutate(undergrad_proportion = Undergrad/(Undergrad + Professional + Grad))
}
