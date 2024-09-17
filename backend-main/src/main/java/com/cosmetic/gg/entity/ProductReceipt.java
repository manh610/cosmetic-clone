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
//@Table(name = "product_receipt", indexes = {
//  @Index(name = "idx_product_receipt_product_id", columnList = "product_id"),
//  @Index(name = "idx_product_receipt_receipt_id", columnList = "receipt_id"),
//  @Index(name = "idx_product_receipt_origin_price", columnList = "origin_price"),
//  @Index(name = "idx_product_receipt_quantity", columnList = "quantity")
//})
//@Entity
//@Getter
//@Setter
//public class ProductReceipt extends EntityBase{
//
//	@NotEmpty(message = "Id of product can not empty")
//	@NotNull(message = "Id of product can not null")
//	@Column(name = "product_id")
//	private Integer productId;
//	
//	@NotEmpty(message = "Id of user not empty")
//	@NotNull(message = "Id of user can not null")
//	@Column(name = "receipt_id")
//	private String receiptId;
//	
//	@Column(name = "origin_price")
//	private boolean originPrice;
//	
//	@Column(name = "quantity")
//	private boolean quantity;
//	
//	@Column(name = "unit")
//	private boolean unit;
//}
