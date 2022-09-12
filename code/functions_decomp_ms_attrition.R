


Umat1 <- matrices$female
Umat2 <- matrices$male

# step 1:
# convert the U matrix to a complete Ptibble: 
# P1 <- from_U_to_trans_matrix(Umat1) %>% 
#        as_tibble() %>% 
#        complete_partial_Ptibble() %>% 
#        select(-HH, -UU)
# P2 <- from_U_to_trans_matrix(Umat2) %>% 
#   as_tibble() %>% 
#   complete_partial_Ptibble() %>% 
#   select(-HH, -UU)
#  # make P2 in the same way
#  cc_p_attrition <-
#    horiuchi2(func = partial_vec_to_ex, 
#              # define vectors in situ
#              pars1 = P1 %>% 
#                partial_Ptibble_to_vec(), 
#              pars2 = P2 %>% 
#                partial_Ptibble_to_vec(), 
#              # how many interpolation steps?
#              N = 20) %>% 
#    vec_to_partial_Ptibble() %>% 
#    rownames_to_column("age") %>% 
#    pivot_longer(2:5, names_to = "from_to", values_to = "cc") %>% 
#    mutate(variant = "prob attrition")
# 
#  cc_p_attrition %>% 
#    mutate(age = as.integer(age) + 50) %>% 
#    ggplot(aes(x = age, y = cc, fill = from_to))+
#    geom_col() +
#    labs(title = "Men worse than women on mortality, but *much* better on health transitions!")
#  
#  cc_p_attrition %>% 
#    count(from_to, wt = cc, name = "contribution")%>% 
#    ggplot(aes(x = from_to, y = contribution, fill = from_to))+
#    geom_col()
# 
#  cc_p_attrition$cc %>% sum()






