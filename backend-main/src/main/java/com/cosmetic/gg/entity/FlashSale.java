//package com.cosmetic.gg.entity;
//
//import java.time.LocalDateTime;
//
//import javax.persistence.Column;
//import javax.persistence.Entity;
//import javax.persistence.GeneratedValue;
//import javax.persistence.Id;
//import javax.persistence.Index;
//import javax.persistence.Lob;
//import javax.persistence.Table;
//import javax.validation.constraints.NotEmpty;
//import javax.validation.constraints.NotNull;
//import javax.validation.constraints.Pattern;
//import javax.validation.constraints.Size;
//
//import org.hibernate.annotations.GenericGenerator;
//
//import com.cosmetic.gg.common.enums.EUserRank;
//
//import lombok.AllArgsConstructor;
//import lombok.Getter;
//import lombok.NoArgsConstructor;
//import lombok.Setter;
//
//@AllArgsConstructor
//@NoArgsConstructor
//@Table(name = "flash_sale", indexes = {
//  @Index(name = "idx_flash_sale_code", columnList = "code"),
//  @Index(name = "idx_flash_sale_name", columnList = "name"),
//  @Index(name = "idx_flash_sale_price_sale", columnList = "price_sale")
//})
//@Entity
//@Getter
//@Setter
//public class FlashSale extends EntityCommon{
//
//	@NotEmpty(message = "Code can not empty")
//	@NotNull(message = "Code can not null")
//	@Size(max = 50, message = "Max length is 50 characters")
//	@Pattern(regexp = "^[a-zA-Z0-9_.-]*$", message = "Code only contains character, number, special character _-.")
//	@Column(name = "code", length = 50)
//	private String code;
//	
//	@NotEmpty(message = "Name can not empty")
//	@NotNull(message = "Name can not null")
//	@Size(max = 700, message = "Max length is 700 characters")
//	@Column(name = "name", length = 700)
//	private String name;
//	
//	@Column(name = "price_sale")
//	private Integer priceSale;
//	
//	@Column(name = "image")
//	@Lob
//    private byte[] image;
//}
