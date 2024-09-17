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
//@Table(name = "product_flash_sale", indexes = {
//  @Index(name = "idx_product_flash_sale_product_id", columnList = "product_id"),
//  @Index(name = "idx_product_flash_sale_flash_sale_id", columnList = "flash_sale_id")
//})
//@Entity
//@Getter
//@Setter
//public class ProductFlashSale extends EntityBase{
//	
//	@NotEmpty(message = "Id of product can not empty")
//	@NotNull(message = "Id of product can not null")
//	@Column(name = "product_id")
//	private Integer productId;
//	
//	@NotEmpty(message = "Id of flash sale can not empty")
//	@NotNull(message = "Id of flash sale can not null")
//	@Column(name = "flash_sale_id")
//	private Integer flashSaleId;
//	
//	@Column(name = "start_date")
//	private LocalDateTime startTime;
//	
//	@Column(name = "end_date")
//	private LocalDateTime endTime;
//}
