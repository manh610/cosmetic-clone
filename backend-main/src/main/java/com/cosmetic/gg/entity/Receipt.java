//package com.cosmetic.gg.entity;
//
//import java.time.LocalDateTime;
//
//import javax.persistence.Column;
//import javax.persistence.Entity;
//import javax.persistence.GeneratedValue;
//import javax.persistence.Id;
//import javax.persistence.Index;
//import javax.persistence.Table;
//import javax.validation.constraints.NotEmpty;
//import javax.validation.constraints.NotNull;
//import javax.validation.constraints.Pattern;
//import javax.validation.constraints.Size;
//
//import org.hibernate.annotations.GenericGenerator;
//
//import lombok.AllArgsConstructor;
//import lombok.Getter;
//import lombok.NoArgsConstructor;
//import lombok.Setter;
//
//@AllArgsConstructor
//@NoArgsConstructor
//@Table(name = "receipt", indexes = {
//  @Index(name = "idx_receipt_representative_name", columnList = "representative_name"),
//  @Index(name = "idx_receipt_representative_email", columnList = "representative_email"),
//  @Index(name = "idx_receipt_representative_phone", columnList = "representative_phone"),
//})
//@Entity
//@Getter
//@Setter
//public class Receipt extends EntityBase{
//	
//	@Column(name = "representative_name")
//	private String representativeName;
//	
//	@Column(name = "representative_email", length = 50, unique = true)
//	@Size(max = 50, message = "Max length is 50 characters")
//	@NotEmpty(message = "Email address can not be empty")
//	@Pattern(
//			regexp = "(^([a-zA-Z0-9_.+\\-])+@(([a-zA-Z0-9\\-])+\\.)+([a-zA-Z0-9]{2,4})+$)",
//			message = "Email must contain character, number, special character _-. and max length 50 characters")
//	private String representativeEmal;
//	
//	@Column(name = "representative_phone", length = 15, unique = true)
//	@Size(max = 15, message = "Max length is 15 characters")
//	@NotNull(message = "Phone number can not be null")
//	@NotEmpty(message = "Phone number can not be empty")
//	@Pattern(
//			regexp = "(^\\d{10,15}$)",
//			message = "Phone number must have min length 10 numbers and max length 15 numbers")
//	private String representativePhone;
//	
//	@Column(name = "description")
//	private String description;
//	
//	@Column(name = "supplier_id")
//	private String supplierId;
//	
//	@Column(name = "branch_id")
//	private String branchId;
//	
//	@Column(name = "import_date")
//	private LocalDateTime importDate;
//	
//	@Column(name = "vat")
//	private Integer vat;
//}
