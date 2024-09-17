package com.cosmetic.gg.entity.product;

import java.time.LocalDateTime;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Index;
import javax.persistence.Lob;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import org.hibernate.annotations.GenericGenerator;

import com.cosmetic.gg.entity.EntityCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "product", indexes = {
    @Index(name = "idx_product_code", columnList = "code"),
    @Index(name = "idx_product_name", columnList = "name")
})
@Entity
@Getter
@Setter
public class Product extends EntityCommon{

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
	
	@Column(name = "photo")
	@Lob
	private byte[] photo;
	
	@Column(name = "made_in")
	private String madeIn;
	
	@Column(name = "production_date")
	private LocalDateTime productionDate;
	
	@Column(name = "expiration_date")
	private LocalDateTime expirationDate;
	
	@Column(name = "category_id")
	private String categoryId;
	
	@Column(name = "brand_id")
	private String brandId;
}
