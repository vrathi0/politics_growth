# Create the final data frame with consistent spelling
# capex_events <- data.frame(
#   event = c(
#     "Industrial entrepreneurs memorandum (IEM) filed",
#     "Letter of Intent (LoI) received",
#     "State government approval sought",
#     "Memorandum of Understanding (MoU) signed",
#     "Forest clearance sought",
#     "Environmental clearance sought",
#     "Expert Appraisal Committee (EAC) recommendation received",
#     "Coastal regulatory zone (CRZ) clearance received",
#     "Special Economic Zone (SEZ) Board in-principle approval received",
#     "Forest clearance received",
#     "Environmental clearance received",
#     "State government approval received",
#     "Central Government approval received",
#     "Cabinet Committee on Economic Affairs (CCEA) approval received",
#     "Land acquisition problem",
#     "Land acquisition problem resolved",
#     "Land acquired",
#     "Land allotment cancelled",
#     "Acquired land returned",
#     "Implementation stalled on",
#     "Issues resolved by Cabinet Committee on Investment (CCI)",
#     "Shelved on",
#     "Abandoned on"
#   ),
#   phase = c(1,1,1,1,
#             2,2,2,2,
#             3,3,3,3,3,3,
#             4,4,4,4,4,
#             5,5,5,5),
#   sub_phase = c(1,2,3,4,
#                 1,2,3,4,
#                 1,2,3,4,5,6,
#                 1,2,3,4,5,
#                 1,2,3,4),
#   event_order = c(1.1,1.2,1.3,1.4,
#                   2.1,2.2,2.3,2.4,
#                   3.1,3.2,3.3,3.4,3.5,3.6,
#                   4.1,4.2,4.3,4.4,4.5,
#                   5.1,5.2,5.3,5.4),
#   phase_name = c(
#     rep("Initial Applications", 4),
#     rep("Initial Environmental & Forest Processes", 4),
#     rep("Major Approvals", 6),
#     rep("Land Related Events", 5),
#     rep("Implementation & Problems", 4)
#   ),
#   frequency = c(5232,1108,688,6125,
#                 1521,10436,3376,260,
#                 226,1148,7919,5471,3411,966,
#                 1263,133,5167,27,5,
#                 4540,143,2827,2299)
# )



# Create expanded data frame with additional events
capex_events <- data.frame(
  event = c(
    # Phase 1: Initial Applications
    "Industrial entrepreneurs memorandum (IEM) filed",
    "Letter of Intent (LoI) received",
    "State government approval sought",
    "Memorandum of Understanding (MoU) signed",
    "Central Electricity Authority (CEA) approval sought",
    
    # Phase 2: Initial Environmental & Forest Processes
    "Forest clearance sought",
    "Environmental clearance sought",
    "Expert Appraisal Committee (EAC) recommendation received",
    "Coastal regulatory zone (CRZ) clearance received",
    "Forest Advisory Committee (FAC) recommendation received",
    
    # Phase 3: Major Initial Approvals
    "Special Economic Zone (SEZ) Board in-principle approval received",
    "Central Electricity Authority (CEA) initial approval received",
    "Central Electricity Authority (CEA) in principle approval received",
    "Planning Commission approval received",
    "Public Investment Board (PIB) approval received",
    "Foreign Investment Promotion Board (FIPB) approval received",
    
    # Phase 4: Major Final Approvals
    "Forest clearance received",
    "Environmental clearance received",
    "State government approval received",
    "Central Government approval received",
    "Cabinet Committee on Economic Affairs (CCEA) approval received",
    "Special Economc Zone (SEZ) Board formal approval received",
    "Special Economic Zone (SEZ) Board notified",
    
    # Phase 5: Land Related Events
    "Land acquisition awaited",
    "Land acquisition problem",
    "Land acquisition problem resolved",
    "Land acquired",
    "Land allotment cancelled",
    "Acquired land returned",
    "De-notification request approved by BoA/SEZ Board",
    
    # Phase 6: Implementation & Problems
    "Implementation stalled on",
    "Issues resolved by Cabinet Committee on Investment (CCI)",
    "Contract type changed",
    "Contract cancelled",
    "Contract termination revoked",
    "Project rejected/deferred by the SEZ Board",
    "Rejected by central government",
    "Shelved on",
    "Abandoned on"
  ),
  phase_order = c(rep(1,5), 
            rep(2,5),
            rep(3,6),
            rep(4,7),
            rep(5,7),
            rep(6,9)),
  sub_phase = c(1:5,
                1:5,
                1:6,
                1:7,
                1:7,
                1:9),
  event_order = paste0(rep(1:6, c(5,5,6,7,7,9)), ".", 
                       c(1:5,1:5,1:6,1:7,1:7,1:9)),
  phase_name = c(
    rep("Initial Applications", 5),
    rep("Initial Environmental & Forest Processes", 5),
    rep("Major Initial Approvals", 6),
    rep("Major Final Approvals", 7),
    rep("Land Related Events", 7),
    rep("Implementation & Problems", 9)
  ),
  frequency = c(5232,1108,688,6125,40,
                1521,10436,3376,260,97,
                226,18,226,361,106,191,
                1148,7919,5471,3411,966,637,438,
                1500,1263,133,5167,27,5,71,
                4540,143,245,568,2,2,19,2827,2299)
)

# Add note about potential inconsistencies
attr(capex_events, "note") <- "Note: Events are ordered chronologically within phases but may occur in different order in practice"
capex_events=capex_events %>% 
  mutate(order=1:n()) %>% 
  dplyr::select(event, order, everything())

#chatgpt

cpx_events= data.frame(
  event = c(
    # Phase 1: Preliminary Filing and Expression of Interest
    "Industrial entrepreneurs memorandum (IEM) filed",
    "Letter of Intent (LoI) sought",
    "Letter of Intent (LoI) received",
    "Memorandum of Understanding (MoU) signed",
    
    # Phase 2: Initial Regulatory Engagement (Application/Preliminary Approvals)
    "State government approval sought",
    "Central Electricity Authority (CEA) approval sought",
    "Forest clearance sought",
    "Environmental clearance sought",
    "Licence sought",
    "Monopolistic & Restrictive Trade Practices (MRTP) clearance sought",
    
    # Phase 3: Detailed Review and Expert Scrutiny
    "Expert Appraisal Committee (EAC) recommendation received",
    "Coastal regulatory zone (CRZ) clearance received",
    "Forest Advisory Committee (FAC) recommendation received",
    "Monopolistic & Restrictive Trade Practices (MRTP) clearance received",
    
    # Phase 4: In-Principle Approvals and Further Governmental Endorsements
    "Special Economic Zone (SEZ) Board in-principle approval received",
    "Central Electricity Authority (CEA) initial approval received",
    "Central Electricity Authority (CEA) in principle approval received",
    "Planning Commission approval received",
    "Public Investment Board (PIB) approval received",
    "Foreign Investment Promotion Board (FIPB) approval received",
    "Special Economc Zone (SEZ) Board formal approval received",
    "Cabinet Committee on Foreign Investment (CCFI) approval received",
    
    # Phase 5: Final Regulatory Clearances
    "Forest clearance received",
    "Environmental clearance received",
    "State government approval received",
    "Central Government approval received",
    "Cabinet Committee on Economic Affairs (CCEA) approval received",
    "Special Economic Zone (SEZ) Board notified",
    "Licence received",
    
    # Phase 6: Land-Related Processes
    "Land acquisition awaited",
    "Land acquisition problem",
    "Land acquisition problem resolved",
    "Land acquired",
    "Land allotment cancelled",
    "Acquired land returned",
    
    # Phase 7: Post-Clearance Contractual and Implementation Adjustments
    "De-notification request approved by BoA/SEZ Board",
    "Issues resolved by Cabinet Committee on Investment (CCI)",
    "Contract type changed",
    "Contract cancelled",
    "Contract termination revoked",
    "Memorandum of Understanding (MoU) cancelled",
    "Project that was transferred from old company",
    "Project that was transferred to a new company",
    
    # Phase 8: Negative or Termination Events
    "Project rejected/deferred by the SEZ Board",
    "Rejected by central government",
    "Shelved on",
    "Abandoned on",
    
    # Phase 9: Additional/Parallel Agreements and Approvals
    "Collaboration approved",
    "Fuel supply agreement (FSA) signed",
    "Power purchase agreement (PPA) cancelled",
    "Power purchase agreement (PPA) signed",
    "Secretariat for Industrial Assistance (SIA) clearance received",
    "Cabinet approval received",
    "Board of Directors' approval received",
    "Licence agreement signed"
  ),
  order = 1:55,
  phase_title = c(
    # Phase 1: Preliminary Filing and Expression of Interest (orders 1-4)
    rep("Preliminary Filing and Expression of Interest", 4),
    # Phase 2: Initial Regulatory Engagement (orders 5-10)
    rep("Initial Regulatory Engagement (Application/Preliminary Approvals)", 6),
    # Phase 3: Detailed Review and Expert Scrutiny (orders 11-14)
    rep("Detailed Review and Expert Scrutiny", 4),
    # Phase 4: In-Principle Approvals and Further Governmental Endorsements (orders 15-22)
    rep("In-Principle Approvals and Further Governmental Endorsements", 8),
    # Phase 5: Final Regulatory Clearances (orders 23-29)
    rep("Final Regulatory Clearances", 7),
    # Phase 6: Land-Related Processes (orders 30-35)
    rep("Land-Related Processes", 6),
    # Phase 7: Post-Clearance Contractual and Implementation Adjustments (orders 36-43)
    rep("Post-Clearance Contractual and Implementation Adjustments", 8),
    # Phase 8: Negative or Termination Events (orders 44-47)
    rep("Negative or Termination Events", 4),
    # Phase 9: Additional/Parallel Agreements and Approvals (orders 48-55)
    rep("Additional/Parallel Agreements and Approvals", 8)
  ),
  
  phase_order = c(
    rep(1, 4),  # Phase 1
    rep(2, 6),  # Phase 2
    rep(3, 4),  # Phase 3
    rep(4, 8),  # Phase 4
    rep(5, 7),  # Phase 5
    rep(6, 6),  # Phase 6
    rep(7, 8),  # Phase 7
    rep(8, 4),  # Phase 8
    rep(9, 8)   # Phase 9
  ),
  
  stringsAsFactors = FALSE
)

# Print the dataframe
print(df)


