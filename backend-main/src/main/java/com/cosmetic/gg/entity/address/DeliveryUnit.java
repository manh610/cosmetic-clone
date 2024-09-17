package com.cosmetic.gg.entity.address;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Index;
import javax.persistence.Lob;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import com.cosmetic.gg.common.enums.EDeliveryType;
import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "delivery_unit", indexes = {
  @Index(name = "idx_delivery_unit_code", columnList = "code"),
  @Index(name = "idx_delivery_unit_name", columnList = "name"),
  @Index(name = "idx_delivery_unit_price", columnList = "price")
})
@Entity
@Getter
@Setter
public class DeliveryUnit extends EntityBase{
	
	@NotEmpty(message = "Code can not empty")
	@NotNull(message = "Code can not null")
	@Size(max = 50, message = "Max length is 50 characters")
	@Pattern(regexp = "^[a-zA-Z0-9_.-]*$", message = "Code only contains character, number, special character _-.")
	@Column(name = "code", length = 50)
	private String code;
	
	@NotEmpty(message = "Name can not empty")
	@NotNull(message = "Name can not null")
	@Size(max = 700, message = "Max length is 700 characters")
	@Column(name = "name", length = 700)
	private String name;
	
	@Column(name = "delivery_type")
	@Enumerated(EnumType.STRING)
	private EDeliveryType deliveryType;
	
	@Column(name = "email", length = 50, unique = true)
	@Size(max = 50, message = "Max length is 50 characters")
	@NotNull(message = "Username can not be null")
	@NotEmpty(message = "Email address can not be empty")
	@Pattern(
			regexp = "(^([a-zA-Z0-9_.+\\-])+@(([a-zA-Z0-9\\-])+\\.)+([a-zA-Z0-9]{2,4})+$)",
			message = "Email must contain character, number, special character _-. and max length 50 characters")
	private String email;
	
	@Column(name = "phone", length = 15, unique = true)
	@Size(max = 15, message = "Max length is 15 characters")
	@NotNull(message = "Phone number can not be null")
	@NotEmpty(message = "Phone number can not be empty")
	private String phone;
	
	@Column(name = "website")
	private String website;
	
	@Column(name = "price")
	private Float price;
	
	@Column(name = "logo")
	@Lob
	private byte[] logo;
	
	@Column(name = "description")
	private String description;
}
